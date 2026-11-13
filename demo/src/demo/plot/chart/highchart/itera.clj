(ns demo.plot.chart.highchart.itera
  (:require [itera.plot.highcharts :as hc]))

(defn economic-activity []
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
                      [280 490] [240 560] [300 430] [390 600]]}]})

(defn annotations []
  {:title {:text "Demo - Annotations"}
   :xAxis {:categories ["Jan" "Feb" "Mar"
                        "Apr" "May" "Jun"
                        "Jul" "Aug" "Sep"
                        "Oct" "Nov" "Dec"]
           :plotBands [{:color "rgba(255,75,66,0.07)"
                        :from 4
                        :to 6
                        :label {:text "forecast"}
                        :zIndex 1000}]}
   :annotations [{:labels [{:point "max" :text "MAX!" :backgroundColor "red"}
                           {:point "min" :text "MIN!" :backgroundColor "white"}
                           {:point {:x 9 :y 150 :xAxis 0 :yAxis 0} :text "label"}]
                  :shapes [{:type "path"
                            :strokeWidth 5
                            :backgroundColor "blue"
                            :fill "blue"
                            :points [{:x 3 :y 105 :xAxis 0 :yAxis 0}
                                     {:x 3 :y 145 :xAxis 0 :yAxis 0}
                                     {:x 5 :y 145 :xAxis 0 :yAxis 0}
                                     {:x 5 :y 105 :xAxis 0 :yAxis 0}]}]}]
   :series [{:data [{:y 29.9 :id "min"}
                    71.5 106.4 129.2 144.0 176.0 135.6 148.5
                    {:y 216.4 :id "max"}
                    194.1 95.6 54.4]}]
   :credits {:enabled false}
   :accessibility {:enabled false}
   :plotOptions {:series {:animation 0}
                 :candlestick {:color "red"
                               :lineColor "red"
                               :upColor "blue"
                               :upLineColor "blue"}}
   :tooltip {:animation {:duration 0}
             :distance 32}
   :navigator {:enabled false}
   :scrollbar {:enabled false}
   :rangeSelector {:enabled false}})

(def examples
  {:highchart/economic-activity
   {:label "Economic activity"
    :data  economic-activity}

   :highchart/annotations
   {:label "Annotations"
    :data  annotations}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :highchart/economic-activity}}]
  (let [{:keys [data]} (get examples example)]
    {:example example :data (data)}))

(defn plot [_opts {:keys [example data]}]
  (case example
    :highchart/economic-activity
    (hc/highchart (assoc data :style {:width "100%" :height "400px"}))

    :highchart/annotations
    (hc/highchart (assoc data :style {:width "100%" :height "400px"}))))

(def algo-highchart
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :highchart/economic-activity}
   :plot/options {}
   :plot/fn plot})
