(ns demo.plot.plotly.itera
  (:require [itera.plot.plotly :as plotly]))

(def surface
  {:layout {:title "A surface plot"}
   :data   [{:z [[1 2 3] [3 2 1]] :type "surface"}]})

(def scatter
  {:layout {:title "A simple scatter plot with lines and markers"}
   :data   [{:x [1 2 3 4] :y [10 15 13 17] :mode "markers" :type "scatter"}
            {:x [2 3 4 5] :y [16 5 11 9] :mode "lines" :type "scatter"}
            {:x [1 2 3 4] :y [12 9 15 12] :mode "lines+markers" :type "scatter"}]})

(def bar
  {:layout {:margin {:l 20 :r 0 :b 20 :t 0}
            :autosize false :width 300 :height 200}
   :config {:displayModeBar false :displayLogo false}
   :data   [{:x ["giraffes" "orangutans" "monkeys"] :y [20 14 23] :type "bar"}]})

(def examples
  {:plotly/surface
   {:label "Surface"
    :data  (fn [] surface)}

   :plotly/scatter
   {:label "Scatter"
    :data  (fn [] scatter)}

   :plotly/bar
   {:label "Bar"
    :data  (fn [] bar)}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :plotly/surface}}]
  (let [{:keys [data]} (get examples example)]
    {:example example :data (data)}))

(defn plot [_opts {:keys [data]}]
  (plotly/plotly (assoc data :style {:width "100%" :height "350px"})))

(def algo-plotly
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :plotly/surface}
   :plot/options {}
   :plot/fn plot})
