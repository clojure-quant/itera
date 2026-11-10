(ns demo.plot.plotly.itera
  (:require [itera.plot.plotly :as plotly]))

(def specs
  [{:layout {:title "A surface plot"}
    :data   [{:z [[1 2 3] [3 2 1]] :type "surface"}]}

   {:layout {:title "A simple scatter plot with lines and markers"}
    :data   [{:x [1 2 3 4] :y [10 15 13 17] :mode "markers" :type "scatter"}
             {:x [2 3 4 5] :y [16 5 11 9] :mode "lines" :type "scatter"}
             {:x [1 2 3 4] :y [12 9 15 12] :mode "lines+markers" :type "scatter"}]}

   {:layout {:margin {:l 20 :r 0 :b 20 :t 0}
             :autosize false :width 300 :height 200}
    :config {:displayModeBar false :displayLogo false}
    :data   [{:x ["giraffes" "orangutans" "monkeys"] :y [20 14 23] :type "bar"}]}])

(defn calculate [_ctx _opts] specs)

(defn plot [_opts specs]
  [:div {:style {:display "grid"
                 :grid-template-columns "1fr 1fr"
                 :gap "1rem"}}
   (for [s specs]
     (plotly/plotly (assoc s :style {:width "100%" :height "350px"})))])

(def algo-plotly
  {:calculate/fn calculate
   :plot/fn      plot})
