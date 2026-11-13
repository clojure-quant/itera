(ns demo.plot.pixi-timeseries.itera
  (:require [itera.plot.pixi-timeseries :as pixi]
            [itera.plot.pixi-timeseries.options]))

(defn- bar [i]
  (let [base  (+ 100 (* 10 (Math/sin (/ i 7.0))) (mod i 5))
        open  (+ base (* 2 (Math/sin (/ i 3.0))))
        close (+ base (* 2 (Math/cos (/ i 5.0))))
        high  (+ (max open close) 1.5)
        low   (- (min open close) 1.5)
        mid   (/ (+ high low) 2.0)]
    {:date (str (.plusDays (java.time.LocalDate/of 2024 1 1) i))
     :open open :high high :low low :close close
     :daily-atr-lower (- mid 3) :daily-atr-upper (+ mid 3) :daily-atr-mid mid
     :p0-low (- low 1) :p1-low (- low 2) :pweek-low (- low 3)
     :p0-high (+ high 1) :p1-high (+ high 2) :pweek-high (+ high 3)}))

(def ds (mapv bar (range 240)))

(def charts
  [[{:type :line :col :daily-atr-lower :color "blue-3"}
    {:type :line :col :daily-atr-upper :color "blue-7"}
    {:type :range :col [:daily-atr-lower :daily-atr-upper] :color "red-3"}
    {:type :line :col :p0-low :color "red-3"}
    {:type :line :col :p1-low :color "red-4"}
    {:type :line :col :pweek-low :color "red-5"}
    {:type :line :col :p0-high :color "red-6"}
    {:type :line :col :p1-high :color "red-7"}
    {:type :line :col :pweek-high :color "red-8"}]
   [{:type :line :col :daily-atr-mid :color "green-5"}]
   [{:type :line :col :low :color "green-2"}
    {:type :point :col :high :color "green-9"}]])

(defn calculate [_ctx _opts] ds)


(def algo-pixi-timeseries
  {:calculate/fn calculate
   :plot/option-initial {:charts charts
                         :step 8
                         :style {:width "100%" :height "800px"}}
   :plot/options  [{:type :select
                    :path [:step]
                    :name "Step"
                    :spec [4 6 8 10 12 14]}
                   {:type :chartspec
                    :path [:charts]
                    :name "ChartSpec"
                    :spec {:columns [:open :high :low :close
                                     :daily-atr-lower :daily-atr-upper :daily-atr-mid
                                     :p0-low :p1-low :pweek-low
                                     :p0-high :p1-high :pweek-high]
                           :colors  ["blue-3" "blue-7" "red-3" "green-2" "green-9"]}}]
   :plot/fn      pixi/pixi-timeseries-ds})
