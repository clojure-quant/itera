(ns itera.discover
  (:require [clojure.java.classpath :as cp]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [resource-seq.core :as rs])
  (:import (java.io File PushbackReader StringReader)
           (java.nio.file ClosedWatchServiceException FileSystems Files
                          LinkOption Path StandardWatchEventKinds WatchEvent
                          WatchService)))

(def ^:private algo-keys
  #{:calculate/options :calculate/fn :plot/options :plot/fn})

(def ^:private ^"[Ljava.nio.file.WatchEvent$Kind;" watch-kinds
  (into-array java.nio.file.WatchEvent$Kind
              [StandardWatchEventKinds/ENTRY_CREATE
               StandardWatchEventKinds/ENTRY_MODIFY
               StandardWatchEventKinds/ENTRY_DELETE]))

(defn- classpath-roots []
  (concat (cp/classpath-directories) (cp/classpath-jarfiles)))

(defn- normalize-path [path]
  (str/replace (str path) "\\" "/"))

(defn- relative-path [roots path]
  (let [path (normalize-path path)]
    (or (some (fn [root]
                (let [prefix (str (normalize-path root) "/")]
                  (when (str/starts-with? path prefix)
                    (subs path (count prefix)))))
              roots)
        path)))

(defn- itera-file? [path]
  (= "itera.clj" (.getName (io/file (str path)))))

(defn- read-ns-sym [content]
  (with-open [r (PushbackReader. (StringReader. content))]
    (loop []
      (let [form (read {:eof ::eof :read-cond :allow} r)]
        (cond
          (= ::eof form) nil
          (and (seq? form) (= 'ns (first form))) (second form)
          :else (recur))))))

(defn- algo-var? [v]
  (let [value (var-get v)]
    (and (map? value) (some #(contains? value %) algo-keys))))

(defn- algos-in-ns [ns-sym file]
  (for [[_ v] (ns-publics ns-sym)
        :when (algo-var? v)]
    {:file file
     :name (str (ns-name (:ns (meta v))) "/" (:name (meta v)))
     :algo v}))

(defn- eval-file [file content]
  (try
    (binding [*ns* *ns* *file* file]
      (load-string content))
    (if-let [ns-sym (read-ns-sym content)]
      {:algos (vec (algos-in-ns ns-sym file)) :error []}
      {:algos [] :error [{:file file :error "no ns form found"}]})
    (catch Throwable e
      {:algos [] :error [{:file file :error (str e)}]})))

(defn- merge-file-result [acc file {:keys [algos error]}]
  (-> acc
      (update :algos (fn [xs] (into (remove #(= file (:file %)) xs) algos)))
      (update :error (fn [xs] (into (remove #(= file (:file %)) xs) error)))))

(defn discover []
  (let [roots (classpath-roots)
        files (->> (rs/resource-seq)
                   (filter (fn [[path _]] (itera-file? path)))
                   (map (fn [[path stream-fn]]
                          [(relative-path roots path)
                           (slurp (stream-fn) :encoding "UTF-8")])))]
    (reduce (fn [acc [file content]]
              (merge-file-result acc file (eval-file file content)))
            {:error [] :algos []}
            files)))

(defn- register-tree [^WatchService ws ^File dir registered]
  (doseq [^File f (file-seq dir)
          :when (.isDirectory f)]
    (let [canon (.getCanonicalPath f)]
      (when-not (contains? @registered canon)
        (swap! registered conj canon)
        (try
          (.register (.toPath f) ws watch-kinds)
          (catch Exception _ nil))))))

(defn- handle-event [roots result ^WatchService ws registered ^Path base ^WatchEvent event]
  (try
    (when-let [ctx (.context event)]
      (let [^Path target (.resolve base ^Path ctx)
            kind (.kind event)]
        (cond
          (and (= kind StandardWatchEventKinds/ENTRY_CREATE)
               (Files/isDirectory target (make-array LinkOption 0)))
          (register-tree ws (.toFile target) registered)

          (itera-file? target)
          (let [rel (relative-path roots target)]
            (if (= kind StandardWatchEventKinds/ENTRY_DELETE)
              (swap! result merge-file-result rel {:algos [] :error []})
              (swap! result merge-file-result
                     rel (eval-file rel (slurp (io/file (.toString target))
                                               :encoding "UTF-8")))))

          :else nil)))
    (catch Throwable _ nil)))

(defn- watch-loop [roots result ^WatchService ws running? registered]
  (try
    (while @running?
      (let [key (.take ws)
            base ^Path (.watchable key)]
        (doseq [ev (.pollEvents key)]
          (handle-event roots result ws registered base ev))
        (.reset key)))
    (catch ClosedWatchServiceException _)
    (catch InterruptedException _)
    (finally
      (.close ws))))

(defn discover-start []
  (let [roots      (classpath-roots)
        result     (atom (discover))
        ws         (-> (FileSystems/getDefault) .newWatchService)
        running?   (atom true)
        registered (atom #{})]
    (doseq [d (cp/classpath-directories)]
      (register-tree ws d registered))
    (alter-meta! result assoc ::watch-service ws ::running? running?)
    (let [task (doto (Thread. #(watch-loop roots result ws running? registered))
                 (.setDaemon true)
                 (.start))]
      {:result result :task task})))

(defn discover-end [state]
  (let [result        (:result state)
        task          (:task state)
        m             (meta result)
        watch-service (::watch-service m)
        running?      (::running? m)]
    (when running? (reset! running? false))
    (when watch-service (.close ^WatchService watch-service))
    (.interrupt task)
    (.join task 2000)
    nil))
