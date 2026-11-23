(ns itera.value.dataset-table-info
  "itera-table-info implementation for tech.ml.dataset / tablecloth datasets.

   Loading this namespace extends `itera.value.protocol/itera-table-info` to
   the tech.ml Dataset class (tablecloth datasets are the same type).

   Column types come from each column's datatype rather than a value scan:
   integer/float datatypes are :number, strings :string, booleans :boolean,
   keywords :keyword, date/time datatypes :time, and anything else :unknown."
  (:require [itera.value.protocol :as protocol]
            [tablecloth.api :as tc]
            [tech.v3.dataset :as ds])
  (:import [tech.v3.dataset.impl.dataset Dataset]))

(defn- datatype->kind
  "Map a tech.ml column datatype keyword to an itera-table-info column kind."
  [datatype]
  (let [n (some-> datatype name)]
    (cond
      (nil? n)                                          :unknown
      (re-find #"^(u?int|long|short|byte|float|double|decimal)" n) :number
      (#{"string" "text"} n)                            :string
      (#{"boolean" "bool"} n)                           :boolean
      (= "keyword" n)                                   :keyword
      (re-find #"date|time|instant|timestamp" n)        :time
      :else                                             :unknown)))

(defn- column-info*
  "Map of column name -> inferred type for dataset D, in column order."
  [d]
  (reduce (fn [m k]
            (assoc m k (datatype->kind (:datatype (meta (ds/column d k))))))
          (array-map)
          (ds/column-names d)))

(extend-protocol protocol/itera-table-info
  Dataset
  (total-rows [d] (ds/row-count d))
  (select-rows [d s e]
    (let [n (long (ds/row-count d))
          s (max 0 (min n (long (or s 0))))
          e (max s (min n (long (or e n))))]
      (tc/select-rows d (range s e))))
  (column-info [d] (column-info* d))
  (as-seq-of-maps [d] (vec (ds/rows d))))