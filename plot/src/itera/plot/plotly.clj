(ns itera.plot.plotly
  "Plotly viewer as a Hyper client component.

   opts: a Plotly spec map, plus `:style` / `:class` host passthrough:
     {:data   [{:x [...] :y [...] :type \"scatter\"} ...]
      :layout {:title \"...\"}
      :config {:displayModeBar false}
      :style  {:width \"100%\" :height \"400px\"}}

   `data` / `layout` / `config` map to Plotly.newPlot's positional args.
   Plotly injects its own styles, so `head` is empty."
  (:require [hyper.core :as h]))

(def head [])

(h/defc itera-plotly
  {:require [["https://esm.sh/plotly.js-dist-min@2.35.2" :as plotly]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [P plotly/default
          el (.querySelector root "div")]
      (set! (.-P ctx) P)
      (set! (.-el ctx) el)
      (.newPlot P el (.-data options) (.-layout options) (.-config options))))
  (update [_root]
    (let [P (.-P ctx)
          el (.-el ctx)]
      (.react P el (.-data options) (.-layout options) (.-config options))))
  (unmount [_]
    (let [P (.-P ctx)
          el (.-el ctx)]
      (when (and P el) (.purge P el)))))

(defn plotly
  [{:keys [style class] :as opts}]
  (itera-plotly (cond-> {:options (dissoc opts :style :class)}
                  style (assoc :style style)
                  class (assoc :class class))))
