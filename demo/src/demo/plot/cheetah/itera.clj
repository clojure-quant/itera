(ns demo.plot.cheetah.itera
  (:require [itera.plot.cheetah :as cheetah]))

(def data
  [{"make" "Toyota"  "model" "Celica" "price" 35000}
   {"make" "Ford"    "model" "Mondeo" "price" 32000}
   {"make" "Porsche" "model" "Boxter" "price" 72000}])

(defn calculate [_ctx _opts] data)

(defn plot [_opts rows]
  (cheetah/cheetah {:columns [{:field "make"  :caption "Make"}
                              {:field "model" :caption "Model"}
                              {:field "price" :caption "Price"}]
                    :data    rows
                    :style   {:width "100%" :height "400px"}}))

(def algo-cheetah
  {:calculate/fn calculate
   :plot/fn      plot})
