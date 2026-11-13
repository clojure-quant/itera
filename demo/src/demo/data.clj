(ns demo.data
  "Runtime CSV loaders for the committed demo datasets.

   Each reader is cached with `delay`, so a file is read and coerced once per
   process.  Values are coerced with an explicit per-file schema."
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.time Instant)))

(defn- parse-value [type s]
  (if (str/blank? s)
    nil
    (case type
      :string  s
      :long    (Long/parseLong s)
      :double  (Double/parseDouble s)
      :boolean (= "true" s)
      :instant (Instant/parse s)
      :edn     (edn/read-string s)
      :keyword (keyword s)
      s)))

(defn- read-csv [resource schema]
  (with-open [r (io/reader (io/resource resource))]
    (let [[header & rows] (csv/read-csv r)
          ks (mapv keyword header)]
      (mapv (fn [row]
              (into {}
                    (map (fn [[k v]]
                           [k (parse-value (get schema k :string) v)]))
                    (map vector ks row)))
            rows))))

(def ^:private bars-schema
  {:asset          :string
   :date           :instant
   :open           :double
   :high           :double
   :low            :double
   :close          :double
   :volume         :double
   :daily-atr-lower :double
   :daily-atr-upper :double
   :daily-atr-mid   :double
   :p0-low         :double
   :p1-low         :double
   :pweek-low      :double
   :p0-high        :double
   :p1-high        :double
   :pweek-high     :double
   :doji           :boolean
   :below-band     :boolean
   :cross-down     :boolean
   :cross-down-c   :boolean
   :long-signal    :boolean
   :above-band     :boolean
   :cross-up       :boolean
   :cross-up-c     :boolean
   :short-signal   :boolean
   :entry          :keyword
   :epoch          :long
   :signal         :boolean
   :pivot          :edn})

(def ^:private roundtrips-schema
  {:entry-idx      :long
   :entry-date     :string
   :entry-price    :double
   :reason         :keyword
   :exit-idx       :long
   :id             :string
   :side           :keyword
   :qty            :double
   :exit-price     :double
   :asset          :string
   :exit-date      :string
   :cum-ret-volume :double
   :cum-ret-prct   :double
   :bars           :long
   :win?           :boolean
   :ret-abs        :double
   :ret-log        :double
   :ret-prct       :double
   :cum-ret-abs    :double
   :cum-ret-log    :double
   :nav            :double})

(def ^:private stocks-schema
  {:symbol :string
   :date   :string
   :price  :double})

(def ^:private bars*
  (delay (read-csv "data/bars-1m-full.csv" bars-schema)))

(def ^:private roundtrips*
  (delay (read-csv "data/roundtrips.csv" roundtrips-schema)))

(def ^:private stocks*
  (delay (read-csv "data/stocks.csv" stocks-schema)))

(defn bars
  "Bars dataset: numeric columns as doubles, :epoch long, :entry keyword."
  []
  @bars*)

(defn roundtrips
  "Roundtrips dataset: index/count columns long, prices/returns/nav double,
   :win? boolean, dates strings."
  []
  @roundtrips*)

(defn stocks
  "Stocks dataset: :price double, :date string, :symbol string."
  []
  @stocks*)
