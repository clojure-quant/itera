(ns demo.plot.table.cheetah.itera
  (:require [demo.data :as data]
            [itera.plot.cheetah :as cheetah]))

(def cars
  [{"make" "Toyota"  "model" "Celica" "price" 35000}
   {"make" "Ford"    "model" "Mondeo" "price" 32000}
   {"make" "Porsche" "model" "Boxter" "price" 72000}])

(defn stocks-rows []
  (mapv (fn [row]
          {"symbol" (:symbol row)
           "date"   (:date row)
           "price"  (:price row)})
        (data/stocks)))

(defn roundtrip-rows []
  (mapv (fn [row]
          {"asset"          (:asset row)
           "id"             (:id row)
           "side"           (name (:side row))
           "qty"            (:qty row)
           "entry-date"     (:entry-date row)
           "entry-idx"      (:entry-idx row)
           "entry-price"    (:entry-price row)
           "exit-date"      (:exit-date row)
           "exit-idx"       (:exit-idx row)
           "exit-price"     (:exit-price row)
           "exit-reason"    (name (:reason row))
           "bars"           (:bars row)
           "win?"           (:win? row)
           "ret-abs"        (:ret-abs row)
           "ret-prct"       (:ret-prct row)
           "ret-log"        (:ret-log row)
           "cum-ret-volume" (:cum-ret-volume row)
           "cum-ret-abs"    (:cum-ret-abs row)
           "cum-ret-log"    (:cum-ret-log row)
           "nav"            (:nav row)})
        (data/roundtrips)))

(def examples
  {:cheetah/cars
   {:label "Cars"
    :data  (fn [] cars)}

   :cheetah/stocks
   {:label "Stocks"
    :data  stocks-rows}

   :cheetah/roundtrips
   {:label "Roundtrips"
    :data  roundtrip-rows}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :cheetah/cars}}]
  (let [{:keys [data]} (get examples example)]
    {:example example :data (data)}))

(defn plot [_opts {:keys [data]}]
  (cheetah/cheetah data))

(def algo-cheetah
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :cheetah/cars}
   :plot/options {}
   :plot/fn plot})
