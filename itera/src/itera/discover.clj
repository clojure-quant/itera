(ns itera.discover
  (:require [clojure.java.classpath :as cp]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [resource-seq.core :as rs])
  (:import (java.io PushbackReader StringReader)))

(def ^:private algo-keys
  #{:calculate/options :calculate/fn :plot/options :plot/fn})

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
  (= "itera.clj" (.getName (io/file path))))

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

(defn- evaluate [file content]
  (try
    (binding [*ns* *ns* *file* file]
      (load-string content))
    (if-let [ns-sym (read-ns-sym content)]
      {:algos (algos-in-ns ns-sym file)}
      {:error {:file file :error "no ns form found"}})
    (catch Throwable e
      {:error {:file file :error (str e)}})))

(defn discover []
  (let [roots (classpath-roots)
        files (->> (rs/resource-seq)
                   (filter (fn [[path _]] (itera-file? path)))
                   (map (fn [[path stream-fn]]
                          [(relative-path roots path)
                           (slurp (stream-fn) :encoding "UTF-8")])))]
    (reduce (fn [acc [file content]]
              (let [result (evaluate file content)]
                (if-let [error (:error result)]
                  (update acc :error conj error)
                  (update acc :algos into (:algos result)))))
            {:error [] :algos []}
            files)))
