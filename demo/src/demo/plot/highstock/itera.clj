(ns demo.plot.highstock.itera
  (:require [demo.data :as data]
            [itera.plot.highcharts :as hc]))

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

(defn- bars-signal-slice
  "A 300-row window that contains :long/:short entry signals."
  []
  (take 300 (drop 3200 (data/bars))))

(defn- with-signal [rows]
  (mapv (fn [row]
          (assoc row :signal (= :long (:entry row))))
        rows))

(defn- with-pivot [rows]
  (mapv (fn [idx row]
          (assoc row :pivot (case idx
                              30 [95.0 100.0 110.0]
                              80 [85.5 99.9 101.1]
                              nil)))
        (range)
        rows))

(defn- with-step-missing [rows]
  (mapv (fn [idx row]
          (assoc row :ind (when-not (<= 20 idx 39)
                            (- (:close row) 2.5))))
        (range)
        rows))

(def examples
  {:highstock/economic-activity
   {:label "Economic activity"
    :data  economic-activity}

   :highstock/ds
   {:label "Dataset"
    :data  (fn []
             {:charts [{:close {:type :line}
                        :bar   {:type :ohlc :mode :candle}}
                       {:volume :column}]
              :rows   (bars-slice)})}

   :highstock/ds-bar
   {:label "Dataset flags"
    :data  (fn []
             {:charts [{:close {:type :line}
                        :bar   {:type :ohlc :mode :candle}
                        :entry {:type      :flags
                                :fillColor "black"
                                :width     10
                                :height    10
                                :v2style   {:long  "url(/itera/plot/flags/arrow-up.svg)"
                                            true   "url(/itera/plot/flags/arrow-down.svg)"
                                            :short "url(/itera/plot/flags/arrow-down.svg)"}}}
                       {:volume :column}]
              :rows   (bars-signal-slice)})}

   :highstock/ds-barcolor
   {:label "Dataset bar color"
    :data  (fn []
             {:charts [{:bar {:type     :ohlc
                              :mode     :candle
                              :barcolor {:column :signal
                                         :color  {true "green-5"}}}}
                       {:volume :column}]
              :rows   (with-signal (bars-signal-slice))})}

   :highstock/ds-pivot
   {:label "Dataset pivot"
    :data  (fn []
             {:charts [{:close {:type :line :color "orange-3"}
                        :bar   {:type :ohlc :mode :candle}
                        :pivot {:type   :point
                                :vec?   true
                                :color  "orange-3"
                                :marker "square"
                                :radius 5}}
                       {:volume :column}]
              :rows   (with-pivot (take 100 (data/bars)))})}

   :highstock/ds-step-missing
   {:label "Dataset step gaps"
    :data  (fn []
             {:charts [{:bar {:type :ohlc :mode :candle}
                        :ind {:type :step :color "red"}}
                       {:volume :column}]
              :rows   (with-step-missing (take 100 (data/bars)))})}})

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

    (:highstock/ds :highstock/ds-bar :highstock/ds-barcolor
     :highstock/ds-pivot :highstock/ds-step-missing)
    (hc/highstock-ds {:style  {:width "100%"}
                      :charts (:charts data)}
                     (:rows data))))

(def algo-highstock
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :highstock/economic-activity}
   :plot/options {}
   :plot/fn plot})
