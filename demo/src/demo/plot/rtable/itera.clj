(ns demo.plot.rtable.itera
  (:require [itera.plot.rtable :as rtable]))

(def columns
  [{:path :close :header "Close" :format (fn [v] (str v))
    :style {:border "1px solid #ccc" :padding "4px 10px"}}
   {:path :sma :header "SMA" :format (fn [v] (str "#" v))
    :style {:border "1px solid #ccc" :padding "4px 10px"}}])

(def data
  [{:close 10.444 :sma 10}
   {:close 14 :sma 12}
   {:close 9 :sma 11}
   {:close 16 :sma 13}
   {:close 11 :sma 12}
   {:close 10 :sma 11}])

(defn calculate [_ctx _opts] data)

(defn plot [_opts rows]
  (rtable/rtable {:columns columns
                  :rows    rows
                  :style   {:border-collapse "collapse"}}))

(def algo-rtable
  {:calculate/fn calculate
   :plot/fn      plot})
