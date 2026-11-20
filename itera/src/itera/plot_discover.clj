(ns itera.plot-discover
  "Discover the assets declared by plot projects.

   Every project that defines plot functions ships a classpath resource
   named `itera-plots.edn` with a map, e.g.

     {:head      [itera.plot.echarts/head
                  itera.plot.plotly/head]
      :option-ui [itera.plot.my-viewer.options]
      :requires  [[itera.plot.echarts :refer [echarts]]]}

   `:head` lists fully-qualified symbols of the project's head vars; they are
   resolved (loading the namespaces, which registers their Hyper client
   components) and their values concatenated.

   `:option-ui` lists namespace symbols whose loading registers
   `itera.option.core/editor` implementations.

   `:requires` lists libspecs (as plain data: a sequential whose first element
   is a namespace symbol) that workbook namespaces refer in so formulas can call
   the project's plot functions without every host repeating them in its own
   `ctx`.

   All keys are optional.  `discover` reads every such resource;
   `itera.hyper/start` requires the option-UI namespaces, resolves the heads and
   merges the `:requires` into the application context."
  (:require [clojure.edn :as edn]
            [resource-seq.core :as rs])
  (:import (java.io File)))

(def resource-name
  "Classpath resource name a plot project uses to declare its assets."
  "itera-plots.edn")

(def ^:private known-keys
  "The keys allowed in an `itera-plots.edn` map."
  #{:head :option-ui :requires})

(defn- resource-files
  "All `itera-plots.edn` resources on the classpath, sorted by path for a
   stable head order."
  []
  (->> (rs/resource-seq)
       (filter (fn [[path _]]
                 (= resource-name (.getName (File. (str path))))))
       (sort-by (fn [[path _]] (str path)))))

(defn- read-entry
  "Parse a resource's EDN map into {:head [...] :option-ui [...] :requires [...]}."
  [path content]
  (let [value (edn/read-string content)]
    (when-not (map? value)
      (throw (ex-info (str resource-name " must contain a map")
                      {:resource path :value value})))
    (when-let [unknown (seq (remove known-keys (keys value)))]
      (throw (ex-info (str resource-name " has unknown keys: " (pr-str (vec unknown)))
                      {:resource path :unknown (vec unknown) :value value})))
    (let [heads    (or (:head value) [])
          opt-uis  (or (:option-ui value) [])
          requires (or (:requires value) [])]
      (when-not (and (sequential? heads) (every? qualified-symbol? heads))
        (throw (ex-info (str resource-name " :head must be a vector of qualified symbols")
                        {:resource path :value (:head value)})))
      (when-not (and (sequential? opt-uis) (every? simple-symbol? opt-uis))
        (throw (ex-info (str resource-name " :option-ui must be a vector of namespace symbols")
                        {:resource path :value (:option-ui value)})))
      (when-not (and (sequential? requires)
                     (every? #(and (sequential? %) (simple-symbol? (first %)))
                             requires))
        (throw (ex-info (str resource-name " :requires must be a vector of libspec forms")
                        {:resource path :value (:requires value)})))
      {:head (vec heads) :option-ui (vec opt-uis) :requires (vec requires)})))

(defn- resolve-head
  "Resolve one head symbol to its value, loading its namespace."
  [sym]
  (try
    (var-get (requiring-resolve sym))
    (catch Throwable e
      (throw (ex-info (str "Cannot resolve plot head symbol " sym)
                      {:symbol sym}
                      e)))))

(defn extension-requires
  "Combine the :requires of a sequence of itera-plots.edn declarations into
   one deduplicated vector, preserving first-seen order."
  [extensions]
  (vec (distinct (mapcat :requires extensions))))

(defn discover
  "Return the raw declarations of every `itera-plots.edn` resource on the
   classpath:
   {:head [<qualified symbols>] :option-ui [<namespace symbols>]
    :requires [<libspecs>]}, deduplicated in resource order."
  []
  (let [entries (map (fn [[path stream-fn]]
                       (read-entry path (slurp (stream-fn) :encoding "UTF-8")))
                     (resource-files))]
    {:head (vec (distinct (mapcat :head entries)))
     :option-ui (vec (distinct (mapcat :option-ui entries)))
     :requires (extension-requires entries)}))

(defn resolve-heads
  "Resolve head symbols to their values, loading their namespaces, and
   concatenate the results into one deduplicated vector."
  [syms]
  (->> (distinct syms)
       (mapv resolve-head)
       (apply concat)
       vec))

(defn require-option-uis!
  "Load every option-UI namespace so its `itera.option.core/editor` methods
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