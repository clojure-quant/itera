(ns demo.plot.chart.highstock.itera
  (:require [demo.data :as data]
            [itera.plot.highcharts :as hc]
            [itera.plot.highcharts-timeseries :as timeseries]))

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
                      [280 490] [240 560] [300 430] [390 600]]}]
   :rangeSelector {:enabled false}
   :navigator     {:enabled false}})

(defn- bars-slice []
  (take 300 (data/bars)))

(def examples
  {:highstock/economic-activity
   {:label "Economic activity"
    :data  economic-activity}

   :highstock/ds
   {:label "Dataset"
    :data  (fn []
             {:rows (bars-slice)})}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :highstock/economic-activity}}]
  (let [{:keys [data]} (get examples example)]
    {:example example :data (data)}))

(defn plot [_opts {:keys [example data]}]
  (case example
    :highstock/economic-activity
    (hc/highstock (assoc data :style {:width "100%" :height "400px"}))

    :highstock/ds
    (timeseries/highcharts-timeseries (:rows data))))

(def algo-highstock
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :highstock/economic-activity}
   :plot/options {}
   :plot/fn plot})