(ns demo.plot.table.aggrid.itera
  (:require [demo.data :as data]
            [itera.plot.aggrid :as aggrid]))

(def cars
  [{"make" "Toyota"  "model" "Celica" "price" 35000}
   {"make" "Ford"    "model" "Mondeo" "price" 32000}
   {"make" "Porsche" "model" "Boxter" "price" 72000}])

(defn unicode-rows []
  (mapv (fn [row action]
          {"date"   (str (:date row))
           "open"   (:open row)
           "action" action})
        (take 10 (data/bars))
        ["😀" "" "" "★" "" "" "❂" "" "" ""]))

(def examples
  {:aggrid/cars
   {:label "Cars"
    :data  (fn [] cars)}

   :aggrid/unicode
   {:label "Unicode"
    :data  unicode-rows}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :aggrid/cars}}]
  (let [{:keys [data]} (get examples example)]
    {:example example :data (data)}))

(defn plot [_opts {:keys [example data]}]
  (case example
    :aggrid/cars
    (aggrid/aggrid {:columnDefs [{:field "make"  :headerName "Make"}
                                 {:field "model" :headerName "Model"}
                                 {:field "price" :headerName "Price"}]
                    :rowData    data
                    :style      {:width "100%" :height "400px"}})

    :aggrid/unicode
    (aggrid/aggrid {:columnDefs [{:field "date"}
                                 {:field "open"}
                                 {:field "action"}]
                    :rowData    data
                    :style      {:width "800px" :height "600px"}})))

(def algo-aggrid
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :aggrid/cars}
   :plot/options {}
   :plot/fn plot})
