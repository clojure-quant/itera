(ns demo.vegalite-specs
  "Dev-only generator that snapshots the Vega-Lite example gallery to EDN.

   Fetches the upstream example index and specs from GitHub, converts each JSON
   spec to deterministic EDN, downloads every referenced dataset, and writes an
   index mapping examples to their specs and datasets.

   Run from demo/:

     clojure -M:vegalite-specs [--ref <ref>] [--out <dir>] [--no-clean]"
  (:require [clojure.data.json :as json]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.walk :as walk])
  (:import (java.io File)
           (java.net URI URLDecoder)
           (java.net.http HttpClient HttpClient$Redirect HttpResponse
                          HttpResponse$BodyHandlers HttpRequest)
           (java.nio.charset StandardCharsets)
           (java.security MessageDigest)
           (java.time Duration)))

;; Config

(def ^:private repo "vega/vega-lite")
(def ^:private default-ref "main")
(def ^:private site-base "https://vega.github.io/vega-lite/")

(def ^:private categories
  [["Single-View Plots" "single-view-plots"]
   ["Composite Marks" "composite-marks"]
   ["Layered Plots" "layered-plots"]
   ["Multi-View Displays" "multi-view-displays"]
   ["Maps (Geographic Displays)" "maps"]
   ["Interactive" "interactive"]])

(defn- examples-json-url [ref]
  (str "https://raw.githubusercontent.com/" repo "/" ref
       "/site/_data/examples.json"))

(defn- spec-url [ref name]
  (str "https://raw.githubusercontent.com/" repo "/" ref
       "/examples/specs/" name ".vl.json"))

;; HTTP

(def ^:private max-attempts 3)

(def ^:private http-client
  (delay
    (-> (HttpClient/newBuilder)
        (.followRedirects HttpClient$Redirect/NORMAL)
        (.connectTimeout (Duration/ofSeconds 30))
        (.build))))

(defn- fetch-once [^HttpClient client ^String url]
  (.send client
         (-> (HttpRequest/newBuilder (URI/create url))
             (.header "User-Agent" "itera-vegalite-specs/1.0")
             (.timeout (Duration/ofSeconds 120))
             (.GET)
             (.build))
         (HttpResponse$BodyHandlers/ofByteArray)))

(defn- fetch-bytes [url]
  (loop [attempt 1]
    (let [outcome (try
                    (let [response (fetch-once @http-client url)
                          status (.statusCode ^HttpResponse response)]
                      (if (<= 200 status 299)
                        {:body (.body ^HttpResponse response)}
                        {:status status}))
                    (catch Exception e
                      {:error e}))]
      (cond
        (contains? outcome :body)
        (do (println "fetched" url) (:body outcome))

        (and (< attempt max-attempts)
             (or (contains? outcome :error)
                 (<= 500 (get outcome :status 0) 599)))
        (do (Thread/sleep (* 500 attempt))
            (recur (inc attempt)))

        :else
        (throw (ex-info (str "failed to fetch " url
                             (when-let [status (:status outcome)]
                               (str " (HTTP " status ")"))
                             (when-let [e (:error outcome)]
                               (str " (" (ex-message e) ")")))
                        {:url url :status (:status outcome)}
                        (:error outcome)))))))

(defn- decode-body [^bytes body]
  (String. body StandardCharsets/UTF_8))

;; JSON -> EDN

(def ^:private keyword-token
  #"[A-Za-z_$][A-Za-z0-9_$*+!?<>=.\-]*")

(defn- json-key [k]
  (if (re-matches keyword-token k)
    (keyword k)
    (throw (ex-info (str "JSON key is not a valid EDN keyword: " (pr-str k))
                    {:key k}))))

(declare json->clj)

(defn- java-map->clj [^java.util.Map m]
  (persistent!
   (reduce (fn [acc ^java.util.Map$Entry entry]
             (assoc! acc
                     (.getKey entry)
                     (json->clj (.getValue entry))))
           (transient {})
           (.entrySet m))))

(defn- json->clj [x]
  (cond
    (instance? java.util.Map x) (java-map->clj x)
    (instance? java.util.List x) (mapv json->clj x)
    :else x))

(defn- parse-examples-json [text]
  (json->clj (json/read-str text)))

(defn- parse-spec-json [text]
  (walk/postwalk
   (fn [x] (if (map? x) (into (sorted-map) x) x))
   (json->clj (json/read-str text :key-fn json-key))))

;; Example selection

(defn- select-examples [examples]
  (reduce
   (fn [acc [category-title slug]]
     (let [subgroups (get examples category-title)]
       (when-not (map? subgroups)
         (throw (ex-info (str "category missing from examples.json: " category-title)
                         {:category category-title})))
       (reduce
        (fn [acc [subcategory entries]]
          (reduce
           (fn [acc {:strs [name title]}]
             (if (contains? (:seen acc) [slug name])
               (do (println "skip duplicate example" (str slug "/" name))
                   acc)
               (-> acc
                   (update :seen conj [slug name])
                   (update :entries conj {:category category-title
                                          :slug slug
                                          :subcategory subcategory
                                          :name name
                                          :title title}))))
           acc
           entries))
        acc
        (sort-by key subgroups))))
   {:seen #{} :entries []}
   categories))

(defn- ordered-entries [entries]
  (mapcat (fn [[_ slug]]
            (->> entries
                 (filter #(= slug (:slug %)))
                 (sort-by :name)))
          categories))

;; Dataset discovery

(defn- spec-urls [spec]
  (let [found (volatile! [])]
    (walk/postwalk
     (fn [x]
       (when (and (map? x) (string? (:url x)))
         (vswap! found conj (:url x)))
       x)
     spec)
    (vec (distinct @found))))

(defn- resolve-source-url [url]
  (cond
    (str/starts-with? url "data:") nil
    (str/starts-with? url "//") (str "https:" url)
    (str/starts-with? url "/") (str "https://vega.github.io" url)
    (re-find #"(?i)^https?://" url) url
    :else (str site-base url)))

(defn- strip-query-and-fragment [url]
  (-> url
      (str/split #"#" 2)
      first
      (str/split #"\?" 2)
      first))

(defn- url-path [url]
  (let [path (strip-query-and-fragment url)]
    (if (re-find #"(?i)^https?://" path)
      (.getPath (URI/create path))
      path)))

(defn- local-path-for [original source]
  (let [path (url-path original)
        path (if (str/starts-with? path "data/")
               path
               (let [absolute (url-path source)]
                 (if (str/starts-with? absolute "/vega-lite/data/")
                   (str "data/" (subs absolute (count "/vega-lite/data/")))
                   (str "data/" (last (str/split absolute #"/"))))))
        decoded (URLDecoder/decode path "UTF-8")]
    (when (or (str/blank? decoded)
              (some #{".." "."} (str/split decoded #"/")))
      (throw (ex-info (str "unsafe dataset path: " (pr-str decoded))
                      {:url original :local-path decoded})))
    decoded))

(defn- sha1-prefix [s]
  (let [digest (.digest (MessageDigest/getInstance "SHA-1")
                        (.getBytes ^String s StandardCharsets/UTF_8))]
    (apply str (map #(format "%02x" %) (take 4 digest)))))

(defn- suffix-filename [path suffix]
  (let [file (io/file path)
        file-name (.getName file)
        dir (.getParent file)
        dot (.lastIndexOf file-name ".")
        stem (if (pos? dot) (subs file-name 0 dot) file-name)
        extension (if (pos? dot) (subs file-name dot) "")
        new-name (str stem "-" suffix extension)]
    (if dir
      (str dir File/separator new-name)
      new-name)))

(defn- unique-local-path [claimed base source]
  (if-not (contains? claimed base)
    base
    (let [suffixed (suffix-filename base (sha1-prefix source))]
      (if-not (contains? claimed suffixed)
        suffixed
        (loop [n 2]
          (let [candidate (suffix-filename base
                                           (str (sha1-prefix source) "-" n))]
            (if (contains? claimed candidate)
              (recur (inc n))
              candidate)))))))

(def ^:private empty-dataset-state
  {:url->local {}
   :local->url {}
   :datasets []})

(defn- plan-datasets [state urls]
  (reduce
   (fn [[state locals] url]
     (if-let [source (resolve-source-url url)]
       (if-let [local (get (:url->local state) source)]
         [state (conj locals local)]
         (let [base (local-path-for url source)
               local (unique-local-path (:local->url state) base source)]
           (when-not (= local base)
             (println "dataset name collision" base "->" local "for" source))
           [(-> state
                (assoc-in [:url->local source] local)
                (assoc-in [:local->url local] source)
                (update :datasets conj {:source source
                                        :url url
                                        :file local}))
            (conj locals local)]))
       [state locals]))
   [state []]
   urls))

;; Writing

(defn- write-edn! [file value]
  (let [file (io/file file)]
    (io/make-parents file)
    (with-open [writer (io/writer file :encoding "UTF-8")]
      (binding [*out* writer
                *print-namespace-maps* false
                pprint/*print-right-margin* 100
                *print-length* nil
                *print-level* nil]
        (pprint/pprint value)))
    (.length file)))

(defn- write-spec! [file spec]
  (let [byte-count (write-edn! file spec)
        reread (edn/read-string (slurp file :encoding "UTF-8"))]
    (when-not (= spec reread)
      (throw (ex-info (str "spec round-trip mismatch: " file)
                      {:file (str file)})))
    (println "wrote" (str file) "(" byte-count "bytes)")
    byte-count))

(defn- download-dataset! [out-dir {:keys [source file]}]
  (let [target (io/file out-dir file)
        ^bytes body (fetch-bytes source)]
    (io/make-parents target)
    (with-open [out (io/output-stream target)]
      (.write out body))
    (println "wrote" (str target) "(" (alength body) "bytes)")
    (alength body)))

;; Output lifecycle

(defn- delete-tree! [file]
  (let [file (io/file file)]
    (when (.exists file)
      (when (.isDirectory file)
        (doseq [child (.listFiles file)]
          (delete-tree! child)))
      (when-not (.delete file)
        (throw (ex-info (str "could not delete " file) {:file (str file)}))))))

(defn- guard-out-dir [out]
  (let [canonical (.getCanonicalPath (io/file out))
        suffix (str File/separator "data" File/separator "vegalite")]
    (when-not (str/ends-with? canonical suffix)
      (throw (ex-info (str "refusing to write outside data/vegalite: " canonical)
                      {:out canonical})))
    (io/file canonical)))

;; Fetching specs

(defn- collect-specs [ref entries]
  (reduce
   (fn [{:keys [state] :as acc} entry]
     (let [url (spec-url ref (:name entry))]
       (try
         (let [spec (-> (fetch-bytes url) decode-body parse-spec-json)
               urls (spec-urls spec)
               [state locals] (plan-datasets state urls)]
           (-> acc
               (assoc :state state)
               (update :specs conj (assoc entry
                                          :spec spec
                                          :urls urls
                                          :data (vec (sort (distinct locals)))))))
         (catch Exception e
           (println "failed spec" (str (:slug entry) "/" (:name entry))
                    "-" (ex-message e))
           (update acc :failures conj {:kind :spec
                                       :slug (:slug entry)
                                       :name (:name entry)
                                       :url url
                                       :message (ex-message e)})))))
   {:specs []
    :state empty-dataset-state
    :failures []}
   entries))

;; Index + consistency

(defn- build-index [ref state specs]
  {:source {:repo repo
            :ref ref
            :examples-json (examples-json-url ref)
            :site-base site-base}
   :categories (mapv (fn [[title slug]] {:title title :slug slug}) categories)
   :datasets (->> (:datasets state)
                  (map #(select-keys % [:file :url :source]))
                  (sort-by :file)
                  vec)
   :specs (mapv (fn [{:keys [category slug subcategory name title data]}]
                  {:category category
                   :slug slug
                   :subcategory subcategory
                   :name name
                   :title title
                   :file (str slug "/" name ".edn")
                   :data data})
                specs)})

(defn- consistency-problems
  [out-dir index spec-records url->local]
  (let [out-dir (io/file out-dir)
        dataset-files (set (map :file (:datasets index)))
        problems (atom [])]
    (doseq [{:keys [file]} (:specs index)]
      (let [spec-file (io/file out-dir file)]
        (if-not (.isFile spec-file)
          (swap! problems conj (str "missing spec file: " file))
          (try
            (edn/read-string (slurp spec-file :encoding "UTF-8"))
            (catch Exception e
              (swap! problems conj
                     (str "unreadable spec file: " file
                          " (" (ex-message e) ")")))))))
    (doseq [{:keys [file]} (:datasets index)]
      (when-not (.isFile (io/file out-dir file))
        (swap! problems conj (str "missing dataset file: " file))))
    (doseq [data-file (sort (set (mapcat :data (:specs index))))]
      (when-not (.isFile (io/file out-dir data-file))
        (swap! problems conj (str "missing data file: " data-file)))
      (when-not (contains? dataset-files data-file)
        (swap! problems conj (str "data file not in :datasets: " data-file))))
    (doseq [{:keys [slug name data urls]} spec-records
            url urls
            :let [source (resolve-source-url url)]
            :when source]
      (let [local (get url->local source)]
        (when-not (and local (some #{local} data))
          (swap! problems conj
                 (str "url not represented in " slug "/" name ": " url)))))
    (doseq [[[slug name] n] (->> (:specs index)
                                 (map (juxt :slug :name))
                                 frequencies
                                 (filter (fn [[_ count]] (> count 1))))]
      (swap! problems conj (str "duplicate spec: " slug "/" name " (" n ")")))
    @problems))

;; CLI / summary

(def ^:private usage-text
  (str "Usage: clojure -M:vegalite-specs [options]\n"
       "\n"
       "Options:\n"
       "  --ref <ref>   git ref of vega/vega-lite to snapshot (default: main)\n"
       "  --out <dir>   output directory (default: ../data/vegalite, run from demo/)\n"
       "  --no-clean    keep the existing output directory\n"
       "  --help        print this help"))

(defn- parse-args [args]
  (loop [args (seq args)
         opts {:ref default-ref
               :out "../data/vegalite"
               :clean? true}]
    (if-let [arg (first args)]
      (cond
        (= arg "--ref")
        (if-let [ref (second args)]
          (recur (nnext args) (assoc opts :ref ref))
          (throw (ex-info "--ref requires a value" {:option arg})))

        (= arg "--out")
        (if-let [out (second args)]
          (recur (nnext args) (assoc opts :out out))
          (throw (ex-info "--out requires a value" {:option arg})))

        (= arg "--no-clean")
        (recur (next args) (assoc opts :clean? false))

        (= arg "--help")
        (do (println usage-text) (System/exit 0))

        :else
        (throw (ex-info (str "unknown option: " arg) {:option arg})))
      opts)))

(defn- print-summary [specs datasets failures problems byte-count]
  (println)
  (doseq [[title slug] categories]
    (println (format "  %-34s %3d specs"
                     title
                     (count (filter #(= slug (:slug %)) specs)))))
  (println (format "  %-34s %3d files" "datasets" (count datasets)))
  (println "bytes written:" byte-count)
  (when (seq failures)
    (println "failures:")
    (doseq [failure failures]
      (println "  -" (pr-str failure))))
  (when (seq problems)
    (println "consistency problems:")
    (doseq [problem problems]
      (println "  -" problem)))
  (println (if (and (empty? failures) (empty? problems))
             "done."
             "incomplete.")))

(defn- generate! [{:keys [ref out clean?]}]
  (let [out-dir (guard-out-dir out)]
    (println "source:" (str "https://github.com/" repo) "at ref" ref)
    (println "output:" (.getCanonicalPath ^File out-dir))
    (when clean?
      (delete-tree! out-dir))
    (let [examples-text (try
                          (-> (fetch-bytes (examples-json-url ref)) decode-body)
                          (catch Exception e
                            (println "fatal: could not fetch examples.json:"
                                     (ex-message e))
                            (System/exit 1)))
          {:keys [entries]} (select-examples (parse-examples-json examples-text))
          ordered (ordered-entries entries)
          _ (println "examples:" (count ordered))
          {:keys [specs state failures]} (collect-specs ref ordered)
          byte-count (atom 0)
          _ (doseq [{:keys [slug name spec]} specs]
              (let [file (io/file out-dir (str slug "/" name ".edn"))]
                (swap! byte-count + (write-spec! file spec))))
          download-failures
          (reduce
           (fn [acc dataset]
             (try
               (swap! byte-count + (download-dataset! out-dir dataset))
               acc
               (catch Exception e
                 (println "failed dataset" (:source dataset) "-" (ex-message e))
                 (conj acc {:kind :dataset
                            :url (:source dataset)
                            :message (ex-message e)}))))
           []
           (:datasets state))
          failures (into failures download-failures)
          index (build-index ref state specs)
          _ (write-edn! (io/file out-dir "index.edn") index)
          problems (consistency-problems out-dir index specs (:url->local state))]
      (print-summary specs (:datasets state) failures problems @byte-count)
      (System/exit (if (or (seq failures) (seq problems)) 1 0)))))

(defn -main [& args]
  (let [opts (try
               (parse-args args)
               (catch clojure.lang.ExceptionInfo e
                 (println (ex-message e))
                 (println usage-text)
                 (System/exit 1)))]
    (try
      (generate! opts)
      (catch Exception e
        (println "error:" (ex-message e))
        (System/exit 1)))))