(ns itera.model-ui.page-model
  "Model page: a viewport-sized Graph/Interact tab layout over one model.

   All model state is owned by itera.runner.ui.  The form-3 view starts the
   model on mount (and keeps it running across tab switches), streams shown
   node values in the interact tab, and stops the model on unmount.  When the
   model file does not exist, it renders a \"Model not found\" page instead.

   The page owns only the tab state and the runner lifecycle.  Graph tab:
   itera.model-ui.graph-editor, which owns the node-edit popup and derives
   every server action from the runner handle."
  (:require [hyper.core :as h]
            [itera.model-ui.graph-editor :as editor]
            [itera.model-ui.interact :as interact]
            [itera.runner.ui :as rui]))

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
          (when-let [this (rui/start-model ctx model-name)]
            (rui/assign-missing-positions this)
            (let [base [:itera-model model-name]
                  tab-a (h/tab-cursor (conj base :tab) "graph")]
              (h/watch! (:model-a this))
              (h/watch! (:index this))
              {:this this
               :tab-a tab-a})))
        :render
        (fn [r _req]
          (if (nil? r)
            [:div.page
             [:h1 "Model not found"]
             [:p {:style {:font-family "monospace"}} model-name]
             [:a (merge {:style {:font-weight "600"}}
                        (h/navigate :itera-model-list))
              "← Models"]]
            (let [{:keys [this tab-a]} r
                  tab (or @tab-a "graph")]
              [:div {:style {:position "fixed"
                             :inset "0"
                             :display "flex"
                             :flex-direction "column"
                             :overflow "hidden"
                             :box-sizing "border-box"
                             :background "#fff"}}
               [:div {:style {:flex "0 0 auto"
                              :display "flex"
                              :align-items "center"
                              :gap "0.25rem"
                              :padding "0.35rem 0.5rem 0"
                              :background "#f8fafc"
                              :border-bottom "1px solid #e2e8f0"
                              :box-sizing "border-box"}}
                [:a (merge {:style {:font-size "0.95rem"
                                    :font-weight "600"
                                    :line-height "1"
                                    :padding "0.35rem 0.5rem"
                                    :color "#334155"
                                    :text-decoration "none"
                                    :cursor "pointer"}
                            :title "Back to models"
                            :aria-label "Back to models"}
                           (h/navigate :itera-model-list))
                 "↑"]
                [:span {:style {:margin-left "0.5rem"
                                :font-weight "600"
                                :font-size "1rem"
                                :color "#0f172a"}}
                 model-name]
                (tab-button tab-a tab "graph")
                (tab-button tab-a tab "interact")
                ]
               (if (= "graph" tab)
                 [:div {:style {:flex "1"
                                :min-height "0"
                                :position "relative"
                                :display "flex"
                                :flex-direction "column"}}
                  (editor/graph-editor this)]
                 (interact/interact-view r))])))
        :unmount (fn [r] (when r (rui/stop-model (:this r))))}))))