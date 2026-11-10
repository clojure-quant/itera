(ns demo.plot.aggrid.itera
  (:require [itera.plot.aggrid :as aggrid]))

(def data
  [{"make" "Toyota"  "model" "Celica" "price" 35000}
   {"make" "Ford"    "model" "Mondeo" "price" 32000}
   {"make" "Porsche" "model" "Boxter" "price" 72000}])

(defn calculate [_ctx _opts] data)

(defn plot [_opts rows]
  (aggrid/aggrid {:columnDefs [{:field "make"  :headerName "Make"}
                               {:field "model" :headerName "Model"}
                               {:field "price" :headerName "Price"}]
                  :rowData    rows
                  :style      {:width "100%" :height "400px"}}))

(def algo-aggrid
  {:calculate/fn calculate
   :plot/fn      plot})
