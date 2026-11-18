(ns itera.page.model
  "Model page: a viewport-sized Graph/Interact tab layout over one model.

   All model state is owned by itera.runner.ui.  The form-3 view starts the
   model on mount (and keeps it running across tab switches), streams shown
   node values in the interact tab, and stops the model on unmount.

   Graph tab: itera.model-ui.graph-editor (a fork of the graph editor).  Editing
   toggles a popup bound to a draft cursor: CodeMirror for :formula nodes and
   itera.input.option for :value nodes; closing the popup writes the node
   through itera.runner.ui."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.input.option :as input-opt]
            [itera.model-ui.graph-editor :as editor]
            [itera.model-ui.interact :as interact]
            [itera.runner.ui :as rui]))

;; ---------------------------------------------------------------------------
;; popup (edit a node's code or option-ui)
;; ---------------------------------------------------------------------------

(defn- open-popup! [r node-id type]
  (let [{:keys [this popup-a code-a rows-a]} r
        node (rui/node-by-id this node-id)]
    (reset! code-a (or (:code node) ""))
    (reset! rows-a (vec (or (:option-ui node) [])))
    (reset! popup-a {:id node-id :label (:label node) :type type})))

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
        {:keys [id label type]} @popup-a
        formula? (= "formula" (name (keyword (or type "value"))))]
    [:div {:style "position:fixed;inset:0;background:rgba(17,24,39,0.45);z-index:1000;display:flex;align-items:center;justify-content:center;padding:1rem"
           :data-on:click (h/action (close-popup! r))}
     [:div {:style "background:white;border-radius:0.5rem;box-shadow:0 10px 30px rgba(0,0,0,0.2);width:min(820px,100%);max-height:92vh;display:flex;flex-direction:column;gap:0.6rem;padding:0.85rem"
            :data-on:click (h/expr (.stopPropagation evt))}
      [:div {:style "font-weight:600;font-size:0.85rem;font-family:monospace"}
       (str "Edit " (or label id) (if formula? " (:formula)" " (:value)"))]
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
             [:div {:style "flex:0 0 auto;display:flex;align-items:center;gap:0.25rem;padding:0.35rem 0.5rem 0;background:#f8fafc;border-bottom:1px solid #e2e8f0;box-sizing:border-box"}
              [:a (merge {:style "font-size:0.95rem;font-weight:600;line-height:1;padding:0.35rem 0.5rem;color:#334155;text-decoration:none;cursor:pointer"
                          :title "Back to models"
                          :aria-label "Back to models"}
                         (h/navigate :itera-model-list))
               "↑"]
              (tab-button tab-a tab "graph")
              (tab-button tab-a tab "interact")
              [:span {:style "margin-left:0.5rem;font-weight:600;font-size:1rem;color:#0f172a"}
               model-name]]
             (if (= "graph" tab)
               [:div {:style "flex:1;min-height:0;position:relative;display:flex;flex-direction:column"}
                (editor/itera-model-graph-editor
                 {:graph (rui/render-graph this)
                  :graph-rev (rui/graph-rev this)
                  :save-result save-result
                  :style "display:block;width:100%;height:100%;min-height:0"
                  :data-on:graph-change
                  (h/action (rui/apply-graph! this (get $detail :graph)
                                              (get $detail :graph-rev)))
                  :data-on:add-node
                  (h/action (rui/add-node! this (keyword (get $detail :type))))
                  :data-on:save
                  (h/action (reset! save-a {:status "ok" :t (System/currentTimeMillis)})
                            (rui/save-model-edn! this))
                  :data-on:edit
                  (h/action (open-popup! r (get $detail :node-id) (get $detail :type)))})
                (when @popup-a
                  (popup-view r))]
               (interact/interact-view r))]))
        :unmount (fn [r] (rui/stop-model (:this r)))}))))
