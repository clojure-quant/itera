(ns itera.plot.echarts
  "ECharts viewer as a Hyper client component.

   The public `echarts` fn splits host-element passthrough (`:style`, `:class`)
   from the ECharts option map and renders the component.

   opts: an ECharts option map, plus `:style` / `:class` host passthrough:
     {:xAxis  {:type \"category\" :data [\"Mon\" ...]}
      :yAxis  {:type \"value\"}
      :series [{:type \"line\" :data [150 230 ...]}]
      :style  {:width \"100%\" :height \"400px\"}}

   ECharts injects its own styles, so `head` is empty."
  (:require [hyper.core :as h]))

(def head [])

(h/defc itera-echarts
  "ECharts instance in a custom element. `options` is the JSON-parsed option map."
  {:require [["https://esm.sh/echarts@5.5.1" :as ec]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [chart (.init ec (.querySelector root "div"))]
      (set! (.-chart ctx) chart)
      (.setOption chart options)))
  (update [_root]
    (when-let [chart (.-chart ctx)]
      (.setOption chart options true)))
  (unmount [_]
    (when-let [chart (.-chart ctx)]
      (.dispose chart))))

(defn echarts
  [{:keys [style class] :as opts}]
  (itera-echarts (cond-> {:options (dissoc opts :style :class)}
                   style (assoc :style style)
                   class (assoc :class class))))
