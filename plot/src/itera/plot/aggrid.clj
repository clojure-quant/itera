(ns itera.plot.aggrid
  "ag-grid (vanilla JS `createGrid`) viewer as a Hyper client component.

   The public `aggrid` fn takes only the row `data`:
     (aggrid [{...} ...])

   Column definitions (the editable spec) come from the host through
   itera.value.protocol; when no spec is given ag-grid auto-generates columns
   from the row data.

   The returned `Aggrid` record implements both itera.value.protocol (renders
   `{:rowData data}`) and itera.value.protocol (`:columnDefs` as the editable
   spec)."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.value.protocol :as spec-plot]))

(def head
  [[:link {:rel  "stylesheet"
           :href "/itera/plot/aggrid/styles/ag-grid.css"}]
   [:link {:rel  "stylesheet"
           :href "/itera/plot/aggrid/styles/ag-theme-quartz.css"}]])

(h/defc itera-aggrid
  "Vanilla ag-grid in a custom element. `options` is the JSON-parsed gridOptions."
  {:require [["https://esm.sh/ag-grid-community@32.3.9" :as ag]]}
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

(defn- render
  "Build the itera-aggrid element from an opts map (ag-grid gridOptions plus
   :style/:class/:theme host passthrough)."
  [{:keys [style class theme] :as opts}]
  (itera-aggrid (cond-> {:options (dissoc opts :style :class :theme)}
                  style (assoc :style style)
                  class (assoc :class class)
                  theme (assoc :theme theme))))

(defrecord Aggrid [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] (render {:rowData data}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (let [style (spec-plot/window->style window)]
      (render (cond-> {:rowData data}
                spec  (assoc :columnDefs spec)
                style (assoc :style style)))))
  (default-spec [_] nil)
  (spec-editor [_ _spec code-a]
    (input-cm/codemirror-ui code-a {:style {:height "440px"}})))

(defn aggrid
  [data]
  (->Aggrid data))
