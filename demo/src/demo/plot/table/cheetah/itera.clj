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

(defn plot [_opts {:keys [example data]}]
  (case example
    :cheetah/cars
    (cheetah/cheetah {:columns [{:field "make"  :caption "Make"}
                                {:field "model" :caption "Model"}
                                {:field "price" :caption "Price"}]
                      :data    data
                      :style   {:width "100%" :height "400px"}})

    :cheetah/stocks
    (cheetah/cheetah {:columns [{:field "symbol" :caption "symbol" :width 90}
                                {:field "date"   :caption "date"   :width 220}
                                {:field "price"  :caption "price"  :width 90}]
                      :data    data
                      :style   {:width "100%" :height "400px"}})

    :cheetah/roundtrips
    (cheetah/cheetah
     {:style   {:width "100%" :height "400px"}
      :columns [{:field "asset"          :caption "a"            :width 90}
                {:field "id"             :caption "id"           :width 50}
                {:field "side"           :caption "side"         :width 50}
                {:field "qty"            :caption "qty"          :width 50}
                {:field "entry-date"     :caption "entry-dt"     :width 160}
                {:field "entry-idx"      :caption "entry-idx"    :width 50}
                {:field "entry-price"    :caption "entry-p"      :width 90}
                {:field "exit-date"      :caption "exit-dt"      :width 160}
                {:field "exit-idx"       :caption "exit-idx"     :width 50}
                {:field "exit-price"     :caption "exit-p"       :width 50}
                {:field "exit-reason"    :caption "exit-reason"  :width 90}
                {:field "bars"           :caption "bars"         :width 50}
                {:field "win?"           :caption "win?"         :width 50}
                {:field "ret-abs"        :caption "ret-abs"      :width 50}
                {:field "ret-prct"       :caption "ret-prct"     :width 50}
                {:field "ret-log"        :caption "ret-log"      :width 50}
                {:field "cum-ret-volume" :caption "cum-ret-volume" :width 50}
                {:field "cum-ret-abs"    :caption "cum-ret-abs"  :width 50}
                {:field "cum-ret-log"    :caption "cum-ret-log"  :width 50}
                {:field "nav"            :caption "nav"          :width 50}]
      :data    data})))

(def algo-cheetah
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :cheetah/cars}
   :plot/options {}
   :plot/fn plot})
