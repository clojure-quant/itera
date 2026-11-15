(ns itera.plot-discover
  "Discover the assets declared by plot projects.

   Every project that defines plot functions ships a classpath resource
   named `itera-plots.edn` with a map, e.g.

     {:head      [itera.plot.echarts/head
                  itera.plot.plotly/head]
      :option-ui [itera.plot.my-viewer.options]}

   `:head` lists fully-qualified symbols of the project's head vars; they are
   resolved (loading the namespaces, which registers their Hyper client
   components) and their values concatenated.

   `:option-ui` lists namespace symbols whose loading registers
   `itera.options/editor` implementations.

   Both keys are optional.  `discover` reads every such resource;
   `itera.hyper/start` requires the option-UI namespaces and resolves the
   heads."
  (:require [clojure.edn :as edn]
            [resource-seq.core :as rs])
  (:import (java.io File)))

(def resource-name
  "Classpath resource name a plot project uses to declare its assets."
  "itera-plots.edn")

(def ^:private known-keys
  "The keys allowed in an `itera-plots.edn` map."
  #{:head :option-ui})

(defn- resource-files
  "All `itera-plots.edn` resources on the classpath, sorted by path for a
   stable head order."
  []
  (->> (rs/resource-seq)
       (filter (fn [[path _]]
                 (= resource-name (.getName (File. (str path))))))
       (sort-by (fn [[path _]] (str path)))))

(defn- read-entry
  "Parse a resource's EDN map into {:head [...] :option-ui [...]}."
  [path content]
  (let [value (edn/read-string content)]
    (when-not (map? value)
      (throw (ex-info (str resource-name " must contain a map")
                      {:resource path :value value})))
    (when-let [unknown (seq (remove known-keys (keys value)))]
      (throw (ex-info (str resource-name " has unknown keys: " (pr-str (vec unknown)))
                      {:resource path :unknown (vec unknown) :value value})))
    (let [heads   (or (:head value) [])
          opt-uis (or (:option-ui value) [])]
      (when-not (and (sequential? heads) (every? qualified-symbol? heads))
        (throw (ex-info (str resource-name " :head must be a vector of qualified symbols")
                        {:resource path :value (:head value)})))
      (when-not (and (sequential? opt-uis) (every? simple-symbol? opt-uis))
        (throw (ex-info (str resource-name " :option-ui must be a vector of namespace symbols")
                        {:resource path :value (:option-ui value)})))
      {:head (vec heads) :option-ui (vec opt-uis)})))

(defn- resolve-head
  "Resolve one head symbol to its value, loading its namespace."
  [sym]
  (try
    (var-get (requiring-resolve sym))
    (catch Throwable e
      (throw (ex-info (str "Cannot resolve plot head symbol " sym)
                      {:symbol sym}
                      e)))))

(defn discover
  "Return the raw declarations of every `itera-plots.edn` resource on the
   classpath: {:head [<qualified symbols>] :option-ui [<namespace symbols>]},
   deduplicated in resource order."
  []
  (let [entries (map (fn [[path stream-fn]]
                       (read-entry path (slurp (stream-fn) :encoding "UTF-8")))
                     (resource-files))]
    {:head (vec (distinct (mapcat :head entries)))
     :option-ui (vec (distinct (mapcat :option-ui entries)))}))

(defn resolve-heads
  "Resolve head symbols to their values, loading their namespaces, and
   concatenate the results into one deduplicated vector."
  [syms]
  (->> (distinct syms)
       (mapv resolve-head)
       (apply concat)
       vec))

(defn require-option-uis!
  "Load every option-UI namespace so its `itera.options/editor` methods
   register.  Throws with the offending symbol when a namespace fails."
  [syms]
  (doseq [sym (distinct syms)]
    (try
      (require sym)
      (catch Throwable e
        (throw (ex-info (str "Cannot require option-ui namespace " sym)
                        {:symbol sym}
                        e)))))
  nil)