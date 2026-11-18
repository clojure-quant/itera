(ns itera.page.model
  "Model page: a viewport-sized Graph/Interact tab layout over one model.

   All model state is owned by itera.runner.ui.  The form-3 view starts the
   model on mount (and keeps it running across tab switches), streams shown
   node values in the interact tab, and stops the model on unmount.

   Graph tab: itera.runner.editor (a fork of the graph editor).  Editing
   toggles a popup bound to a draft cursor: CodeMirror for :formula nodes and
   itera.input.option for :value nodes; closing the popup writes the node
   through itera.runner.ui."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.input.option :as input-opt]
            [itera.option.core :as options]
            [itera.runner.editor :as editor]
            [itera.runner.ui :as rui]))

;; ---------------------------------------------------------------------------
;; interact tab
;; ---------------------------------------------------------------------------

(defn- hiccup-value? [v]
  (or (string? v)
      (and (vector? v) (keyword? (first v)))))

(defn- render-value [a]
  (if a
    (h/reactive [a]
                (let [v @a]
                  (when (hiccup-value? v) v)))
    [:span {:style {:color "#94a3b8"}} "—"]))

(defn- interact-view [r]
  (let [{:keys [this]} r
        index-a (:index this)
        option-nodes (rui/option-ui-nodes this)]
    [:div {:style "flex:1;min-height:0;display:flex"}
     [:div {:class "model-sidebar"
            :style "flex:0 0 300px;max-width:300px;min-width:0;overflow-y:auto;border-right:1px solid #e2e8f0;padding:0.75rem"}
      (if (seq option-nodes)
        (for [{:keys [node-id spec value]} option-nodes]
          (let [base (if (map? value) value {})]
            [:div {:key node-id :style "margin-bottom:1.25rem"}
             [:div {:style "font-weight:600;font-size:0.8rem;margin-bottom:0.35rem;font-family:monospace"}
              node-id]
             (options/options-ui
              {:group (keyword "model" node-id)
               :spec spec
               :value value
               :set-fn (fn [p v] (rui/set-node-value this node-id (assoc-in base p v)))
               :update-fn (fn [p f] (rui/set-node-value this node-id (update-in base p f)))})]))
        [:div {:style "color:#64748b;font-size:0.8rem"}
         "No value nodes with an option-ui spec."])]
     [:div {:class "model-values"
            :style "flex:1;min-width:0;overflow-y:auto;padding:0.75rem"}
      [:h3 {:style "margin:0 0 0.5rem;font-size:0.95rem"} "Values"]
      (if (seq @index-a)
        (for [node-id (sort @index-a)]
          (let [a (rui/get-atom this node-id)]
            [:div {:key node-id
                   :style "display:flex;gap:1rem;align-items:baseline;border-bottom:1px solid #f1f5f9;padding:0.3rem 0"}
             [:div {:style "flex:0 0 12rem;font-weight:600;font-family:monospace;font-size:0.8rem"}
              node-id]
             [:div {:style "font-size:0.9rem"} (render-value a)]]))
        [:div {:style "color:#64748b;font-size:0.8rem"} "No shown nodes."])]]))

;; ---------------------------------------------------------------------------
;; popup (edit a node's code or option-ui)
;; ---------------------------------------------------------------------------

(defn- open-popup! [r node-id type]
  (let [{:keys [this popup-a code-a rows-a]} r
        node (rui/node-by-id this node-id)]
    (reset! code-a (or (:code node) ""))
    (reset! rows-a (vec (or (:option-ui node) [])))
    (reset! popup-a {:id node-id :type type})))

(defn- close-popup! [r]
  (let [{:keys [this popup-a code-a rows-a]} r
        {:keys [id type]} @popup-a]
    (when id
      (if (= "formula" (name (keyword (or type "value"))))
        (rui/set-node-code this id @code-a)
        (rui/set-node-option-ui this id @rows-a)))
    (reset! popup-a nil)))

(defn- popup-view [r]
  (let [{:keys [popup-a code-a rows-a]} r
        {:keys [id type]} @popup-a
        formula? (= "formula" (name (keyword (or type "value"))))]
    [:div {:style "position:fixed;inset:0;background:rgba(17,24,39,0.45);z-index:1000;display:flex;align-items:center;justify-content:center;padding:1rem"
           :data-on:click (h/action (close-popup! r))}
     [:div {:style "background:white;border-radius:0.5rem;box-shadow:0 10px 30px rgba(0,0,0,0.2);width:min(820px,100%);max-height:92vh;display:flex;flex-direction:column;gap:0.6rem;padding:0.85rem"
            :data-on:click (h/expr (.stopPropagation evt))}
      [:div {:style "font-weight:600;font-size:0.85rem;font-family:monospace"}
       (str "Edit " id (if formula? " (:formula)" " (:value)"))]
      (if formula?
        (input-cm/codemirror-ui code-a {:style {:height "440px"}})
        (input-opt/option-input rows-a))
      [:div {:style "display:flex;justify-content:flex-end;gap:0.5rem"}
       [:button {:type "button"
                 :data-on:click (h/action (close-popup! r))
                 :style "font-size:0.8rem;padding:0.3rem 0.8rem;border:1px solid #2563eb;background:#2563eb;color:white;border-radius:0.375rem;cursor:pointer;font-weight:600"}
        "Close"]]]]))

;; ---------------------------------------------------------------------------
;; page
;; ---------------------------------------------------------------------------

(defn- tab-button [tab-a current label]
  [:button {:type "button"
            :key label
            :data-on:click (h/action (reset! tab-a label))
            :style (merge {:font-size "0.8rem"
                           :padding "0.35rem 0.9rem"
                           :border "1px solid #cbd5e1"
                           :border-top "none"
                           :background (if (= label current) "#fff" "#eef2f7")
                           :color "#334155"
                           :cursor "pointer"
                           :font-weight (if (= label current) "600" "400")})}
   label])

(defn model-page
  "Form-3 view for one model.  Resolves the model name from the route, starts
   the runner on mount, and keeps it running while the graph/interact tabs
   switch (the tab is server-side state, not two separate views)."
  [ctx]
  (fn [req]
    (let [model-name (get-in req [:hyper/route :path-params :model-name])]
      (h/view
       {:mount
        (fn []
          (let [this (rui/start-model ctx model-name)
                _ (rui/assign-missing-positions this)
                base [:itera-model model-name]
                tab-a (h/tab-cursor (conj base :tab) "graph")
                popup-a (h/tab-cursor (conj base :popup) nil)
                code-a (h/tab-cursor (conj base :code) "")
                rows-a (h/tab-cursor (conj base :rows) [])
                save-a (h/tab-cursor (conj base :save) nil)]
            (h/watch! (:model-a this))
            (h/watch! (:index this))
            {:this this
             :tab-a tab-a
             :popup-a popup-a
             :code-a code-a
             :rows-a rows-a
             :save-a save-a}))
        :render
        (fn [r _req]
          (let [{:keys [this tab-a popup-a save-a]} r
                tab (or @tab-a "graph")
                save-result @save-a]
            [:div {:style "position:fixed;inset:0;display:flex;flex-direction:column;overflow:hidden;box-sizing:border-box;background:#fff"}
             [:div {:style "height:2rem;flex:0 0 auto;display:flex;align-items:center;gap:1rem;padding:0 0.75rem;border-bottom:1px solid #e2e8f0;background:#fff;box-sizing:border-box"}
              [:span {:style "font-weight:600;font-size:0.9rem"} (str "Model: " model-name)]
              [:a (merge {:style "font-size:0.78rem;font-weight:600"}
                         (h/navigate :itera-model-list))
               "← Models"]]
             [:div {:style "flex:0 0 auto;display:flex;gap:0.25rem;padding:0.35rem 0.5rem 0;background:#f8fafc;border-bottom:1px solid #e2e8f0;box-sizing:border-box"}
              (tab-button tab-a tab "graph")
              (tab-button tab-a tab "interact")]
             (if (= "graph" tab)
               [:div {:style "flex:1;min-height:0;position:relative;display:flex;flex-direction:column"}
                (editor/itera-model-graph-editor
                 {:graph (rui/render-graph this)
                  :graph-rev (rui/graph-rev this)
                  :save-result save-result
                  :style "display:block;width:100%;height:100%;min-height:0"
                  :data-on:graph-change
                  (h/action (rui/apply-graph! this (get $detail :graph)))
                  :data-on:save
                  (h/action (reset! save-a {:status "ok" :t (System/currentTimeMillis)})
                            (rui/save-model-edn! this))
                  :data-on:edit
                  (h/action (open-popup! r (get $detail :node-id) (get $detail :type)))})
                (when @popup-a
                  (popup-view r))]
               (interact-view r))]))
        :unmount (fn [r] (rui/stop-model (:this r)))}))))
