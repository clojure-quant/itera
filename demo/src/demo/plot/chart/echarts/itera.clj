(ns demo.plot.chart.echarts.itera
  (:require [itera.plot.echarts :as echarts]))

(def line
  {:xAxis  {:type "category"
            :data ["Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"]}
   :yAxis  {:type "value"}
   :series [{:data [150 230 224 218 135 147 260]
             :type "line"}]})

(def bar
  {:xAxis  {:type "category"
            :data ["A" "B" "C" "D" "E"]}
   :yAxis  {:type "value"}
   :series [{:data [28 55 43 91 81]
             :type "bar"}]})

(def pie
  {:series [{:type "pie"
             :radius "60%"
             :data [{:value 28 :name "A"}
                    {:value 55 :name "B"}
                    {:value 43 :name "C"}
                    {:value 91 :name "D"}
                    {:value 81 :name "E"}]}]})

(def examples
  {:echarts/line
   {:label "Line"
    :data  (fn [] line)}

   :echarts/bar
   {:label "Bar"
    :data  (fn [] bar)}

   :echarts/pie
   {:label "Pie"
    :data  (fn [] pie)}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :echarts/line}}]
  (let [{:keys [data]} (get examples example)]
    {:example example :data (data)}))

(defn plot [_opts {:keys [data]}]
  (echarts/echarts (assoc data :style {:width "100%" :height "400px"})))

(def algo-echarts
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :echarts/line}
   :plot/options {}
   :plot/fn plot})
