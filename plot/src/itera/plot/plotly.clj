(ns itera.plot.plotly
  "Plotly viewer as a Hyper client component.

   The public `plotly` fn takes only the `:data` trace vector:
     (plotly [{:x [...] :y [...] :type \"scatter\"} ...])

   `data` maps to Plotly.newPlot's first positional arg.  The non-data
   presentation (the editable spec) comes from the host through
   itera.plot-ui.protocol as `{:layout ... :config ...}`, mapping to
   Plotly.newPlot's remaining positional args; `default-spec` is `nil`, so a
   bare value renders the traces with Plotly's default layout.

   The returned `Plotly` record implements both itera.value.protocol (renders
   the traces) and itera.plot-ui.protocol (`{:layout ... :config ...}` as the
   editable spec).  When rendered through itera-spec-plot the container
   `window` {:width <px> :height <px>} is merged into the spec's `:layout`
   (with `:autosize false`) and the host element is sized from it, so the fixed
   interact window overrides any `:width`/`:height` set in the spec.

   Plotly injects its own styles, so `head` is empty."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.plot-ui.protocol :as spec-plot]
            [itera.value.protocol]))

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

(defn- render
  [{:keys [style class] :as opts}]
  (itera-plotly (cond-> {:options (dissoc opts :style :class)}
                  style (assoc :style style)
                  class (assoc :class class))))

(defrecord Plotly [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] (render {:data data}))
  spec-plot/itera-spec-plot
  (to-hiccup [_ spec window]
    (let [layout (cond-> (or (:layout spec) {})
                   (and (:width window) (:height window))
                   (assoc :width (:width window) :height (:height window)
                          :autosize false))]
      (render (cond-> {:data data}
                (seq layout)   (assoc :layout layout)
                (:config spec) (assoc :config (:config spec))
                window         (assoc :style (spec-plot/window->style window))))))
  (default-spec [_] nil)
  (spec-editor [_ _spec code-a]
    (input-cm/codemirror-ui code-a {:style {:height "440px"}})))

(defn plotly [data]
  (->Plotly data))
