(ns demo.plot.fipp.itera
  (:require [itera.plot.fipp :as fipp]))

(def data
  {:portfolio {:nav       1234567.89
               :positions [{:symbol "AAPL" :qty 100 :price 192.53}
                           {:symbol "MSFT" :qty  50 :price 421.10}
                           {:symbol "GOOG" :qty  25 :price 141.80}]
               :history   [1 2 3 5 8 13 21]}
   :generated "2024-01-01T09:30"})

(defn calculate [_ctx _opts] data)

(defn plot [_opts value]
  (fipp/fipp value {:style {:background     "#f6f6f6"
                            :padding        "0.75rem"
                            :border-radius  "0.375rem"
                            :overflow       "auto"}}))

(def algo-fipp
  {:calculate/fn calculate
   :plot/fn      plot})
