(ns itera.plot.aggrid
  "ag-grid (vanilla JS `createGrid`) viewer as a Hyper client component.

   The public `aggrid` fn splits host-element passthrough (`:style`, `:class`,
   `:theme`) from the ag-grid gridOptions payload and renders the component.

   opts: ag-grid gridOptions, e.g.
     {:columnDefs [{:field \"make\"} ...]
      :rowData    [{...} ...]
      :style      {:width \"100%\" :height \"400px\"}
      :class      nil
      :theme      \"quartz\" (ag-grid legacy theme name; default quartz)}"
  (:require [hyper.core :as h]))

(def head
  [[:link {:rel  "stylesheet"
           :href "https://cdn.jsdelivr.net/npm/ag-grid-community@31.3.2/styles/ag-theme-quartz.css"}]])

(h/defc itera-aggrid
  "Vanilla ag-grid in a custom element. `options` is the JSON-parsed gridOptions."
  {:require [["https://esm.sh/ag-grid-community@31.3.2" :as ag]]}
  [{:keys [options theme]}]
  (render
   [:div {:class (str "ag-theme-" (or theme "quartz"))
          :style "width:100%;height:100%"}])
  (mount [root]
    (let [el (.querySelector root "div")]
      (set! (.-grid ctx) (ag/createGrid el options))))
  (update [root]
    ;; First cut: re-create the grid on any attribute change.
    (when-let [g (.-grid ctx)] (.destroy g))
    (let [el (.querySelector root "div")]
      (set! (.-grid ctx) (ag/createGrid el options))))
  (unmount [_]
    (when-let [g (.-grid ctx)] (.destroy g))))

(defn aggrid
  [{:keys [style class theme] :as opts}]
  (itera-aggrid (cond-> {:options (dissoc opts :style :class :theme)}
                  style (assoc :style style)
                  class (assoc :class class)
                  theme (assoc :theme theme))))
