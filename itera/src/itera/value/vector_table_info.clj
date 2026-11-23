(ns itera.value.vector-table-info
  "itera-table-info implementation for Clojure vectors and seqs.

   A table is a sequence of row maps.  Loading this namespace extends
   `itera.value.protocol/itera-table-info` to `clojure.lang.IPersistentVector`
   and `clojure.lang.ISeq` (lazy seqs, lists, conses, array seqs, ranges).

   Column discovery keeps the first-seen key order of the row maps; type
   inference scans each column's non-nil values, so a column is :nil when it
   has only nil values, :mixed when its values fall into more than one kind,
   and :unknown for values outside the known kinds.  Seq access realizes the
   seq."
  (:require [itera.value.protocol :as protocol]))

(defn- kind*
  "Classify VALUE into a column kind: :nil, :number, :string, :boolean,
   :keyword, :time or :unknown."
  [v]
  (cond
    (nil? v) :nil
    (number? v) :number
    (string? v) :string
    (boolean? v) :boolean
    (keyword? v) :keyword
    (instance? java.time.temporal.Temporal v) :time
    (instance? java.util.Date v) :time
    :else :unknown))

(defn- data-columns*
  "Distinct map keys across ROWS, in first-seen order."
  [rows]
  (reduce (fn [ks row]
            (if (map? row)
              (reduce (fn [ks k]
                        (if (some #(= k %) ks) ks (conj ks k)))
                      ks (keys row))
              ks))
          [] rows))

(defn- column-type*
  "Inferred type of column K over ROWS; nil values are ignored."
  [rows k]
  (let [kinds (reduce (fn [s row]
                        (if (map? row)
                          (let [kind (kind* (get row k))]
                            (if (= kind :nil) s (conj s kind)))
                          s))
                      #{}
                      rows)]
    (cond
      (empty? kinds)     :nil
      (= 1 (count kinds)) (first kinds)
      :else              :mixed)))

(defn- column-info*
  "Map of column name -> inferred type for ROWS, in first-seen column order
   (an array-map, so the order stays insertion order up to the array-map
   threshold and may become hash order beyond it)."
  [rows]
  (reduce (fn [m k] (assoc m k (column-type* rows k)))
          (array-map)
          (data-columns* rows)))

(defn data-columns
  "Distinct column names across DATA, in first-seen order."
  [data]
  (data-columns* data))

(defn column-info
  "Map of column name -> inferred type for DATA."
  [data]
  (column-info* data))

(defn- clamp-range
  "Clamp the half-open range [s e) into [0 n]."
  [n s e]
  (let [n (long n)
        s (max 0 (min n (long (or s 0))))
        e (max s (min n (long (or e n))))]
    [s e]))

(extend-protocol protocol/itera-table-info
  clojure.lang.IPersistentVector
  (total-rows [v] (count v))
  (select-rows [v s e]
    (let [[s e] (clamp-range (count v) s e)]
      (subvec v s e)))
  (column-info [v] (column-info* v))
  (as-seq-of-maps [v] v)

  clojure.lang.ISeq
  (total-rows [v] (count v))
  (select-rows [v s e]
    (let [v (vec v)
          [s e] (clamp-range (count v) s e)]
      (subvec v s e)))
  (column-info [v] (column-info* v))
  (as-seq-of-maps [v] v))