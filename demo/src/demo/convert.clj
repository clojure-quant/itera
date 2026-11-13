(ns demo.convert
  "Dev-only converter: reads the Dali transit-json demo datasets and writes
   committed CSV resources under demo/resources/data.

   Run from demo/:

     clojure -M:convert"
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [tech.v3.dataset :as ds]
            [tech.v3.libs.clj-transit :as transit])
  (:import (java.time Instant LocalDate LocalDateTime ZonedDateTime)
           (java.time.format DateTimeFormatter)))

(def ^:private sources
  {:bars       "/home/florian/repo/pink-gorilla/dali-table/dali-table/resources/public/data/bars-1m-full.transit-json"
   :roundtrips "/home/florian/repo/pink-gorilla/dali-table/dali-table/resources/rtable-demo-data/LWhgL6-roundtrips.transit-json"
   :stocks     "/home/florian/repo/pink-gorilla/dali-table/demo/resources/public/stocks.transit-json"})

(def ^:private outputs
  {:bars       "resources/data/bars-1m-full.csv"
   :roundtrips "resources/data/roundtrips.csv"
   :stocks     "resources/data/stocks.csv"})

(def ^:private roundtrip-date-format
  (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm"))

(defn- read-dataset [path]
  (with-open [in (io/input-stream path)]
    (transit/transit->dataset in :json
                              {:handlers transit/java-time-write-handlers})))

(defn- ->epoch-ms [v]
  (cond
    (instance? Instant v)         (.toEpochMilli ^Instant v)
    (instance? ZonedDateTime v)   (.toEpochMilli (.toInstant ^ZonedDateTime v))
    (instance? LocalDateTime v)   (.toEpochMilli (.toInstant ^LocalDateTime v java.time.ZoneOffset/UTC))
    (instance? LocalDate v)       (.toEpochMilli (.toInstant (.atStartOfDay ^LocalDate v) java.time.ZoneOffset/UTC))
    (number? v)                   (long v)
    (string? v)                   (try (.toEpochMilli (Instant/parse v))
                                       (catch Exception _ nil))
    :else                         nil))

(defn- normalize-value [v]
  (cond
    (nil? v)                    ""
    (instance? Instant v)       (str v)
    (instance? ZonedDateTime v) (str (.toInstant ^ZonedDateTime v))
    (instance? LocalDateTime v) (str v)
    (instance? LocalDate v)     (str v)
    (boolean? v)                (if v "true" "false")
    (keyword? v)                (name v)
    (vector? v)                 (pr-str v)
    (sequential? v)             (pr-str (vec v))
    :else                       v))

(defn- roundtrip-value [k v]
  (if (and (#{:entry-date :exit-date} k) (instance? Instant v))
    (.format roundtrip-date-format (.atZone ^Instant v java.time.ZoneOffset/UTC))
    (normalize-value v)))

(defn- write-csv! [out cols rows val-fn]
  (io/make-parents out)
  (with-open [w (io/writer out)]
    (csv/write-csv w
                   (cons (mapv name cols)
                         (map (fn [row]
                                (mapv #(val-fn % (get row %)) cols))
                              rows))))
  (println "wrote" out "(" (count rows) "rows )"))

(defn- convert! [src out {:keys [row-fn cols-fn val-fn]}]
  (let [d    (read-dataset src)
        cols (cols-fn d)
        rows (mapv row-fn (ds/mapseq-reader d))]
    (write-csv! out cols rows (or val-fn (fn [_ v] (normalize-value v))))))

(defn -main [& _]
  (convert! (:bars sources) (:bars outputs)
            {:row-fn  (fn [row] (assoc row :epoch (->epoch-ms (:date row))))
             :cols-fn (fn [d] (conj (vec (ds/column-names d)) :epoch))})
  (convert! (:roundtrips sources) (:roundtrips outputs)
            {:row-fn  identity
             :cols-fn ds/column-names
             :val-fn  roundtrip-value})
  (convert! (:stocks sources) (:stocks outputs)
            {:row-fn  identity
             :cols-fn ds/column-names})
  (println "done."))
