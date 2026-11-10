(ns demo.plot.highstock.itera
  (:require [itera.plot.highcharts :as hc]))

(defn chart-config []
  {:chart    {:type "line" :animation false}
   :title    {:text "Economic Activity"}
   :subtitle {:text "made with Love"}
   :xAxis    {:categories ["Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"]}
   :yAxis    {:min 0 :title {:text "" :align "high"}}
   :legend   {:layout "vertical" :align "right" :verticalAlign "top"}
   :credits  {:enabled false}
   :tooltip  {:valueSuffix " %"}
   :series   [{:name "Installation" :data [439 523 577 698 931 1131 1333 1175]}
              {:name "Manufacturing" :data [249 244 292 291 390 302 381 404]}
              {:name "Sales & Distribution" :data [117 172 165 191 285 247 321 393]}
              {:name "range" :type "arearange"
               :data [[100 320] [120 350] [160 370] [180 395]
                      [280 490] [240 560] [300 430] [390 600]]}]
   ;; highstock specific:
   :rangeSelector {:enabled false}
   :navigator     {:enabled false}})

(defn calculate [_ctx _opts] (chart-config))

(defn plot [_opts spec]
  (hc/highstock (assoc spec :style {:width "100%" :height "400px"})))

(def algo-highstock
  {:calculate/fn calculate
   :plot/fn      plot})
