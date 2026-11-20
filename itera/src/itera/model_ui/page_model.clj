(ns itera.model-ui.page-model
  "Model page: a viewport-sized Ns/Graph/Interact tab layout over one model.

   All model state is owned by itera.model.ui.  The form-3 view starts the
   model on mount (and keeps it running across tab switches), streams shown
   node values in the interact tab, and stops the model on unmount.  When the
   model file does not exist, it renders a \"Model not found\" page instead.

   The page owns only the tab state, the ns-editor text, and the runner
   lifecycle.  The ns tab edits :ns-data in CodeMirror and evaluates it when
   the tab is left.  Graph tab: itera.model-ui.graph-editor, which owns the
   node-edit popup and derives every server action from the runner handle."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.model-ui.graph-editor :as editor]
            [itera.model-ui.interact :as interact]
            [itera.model.ui :as rui]))

;; ---------------------------------------------------------------------------
;; page
;; ---------------------------------------------------------------------------

(defn- ns-code-change!
  "Keep the editor cursor current; apply code that arrives after the tab was
   left (CodeMirror's blur POST can be processed after the tab-switch action)."
  [{:keys [this tab-a ns-code-a]} code]
  (reset! ns-code-a code)
  (when (not= "ns" @tab-a)
    (rui/set-ns-data! this code)))

(defn- switch-tab!
  "Evaluate :ns-data when the ns tab is left."
  [{:keys [this tab-a ns-code-a]} label]
  (when (and (= "ns" @tab-a) (not= "ns" label))
    (rui/set-ns-data! this @ns-code-a))
  (reset! tab-a label))

(defn- tab-button [r current label]
  [:button {:type "button"
            :key label
            :data-on:click (h/action (switch-tab! r label))
            :style (merge {:font-size "0.8rem"
                           :padding "0.35rem 0.9rem"
                           :border "1px solid #cbd5e1"
                           :border-top "none"
                           :background (if (= label current) "#fff" "#eef2f7")
                           :color "#334155"
                           :cursor "pointer"
                           :font-weight (if (= label current) "600" "400")})}
   label])

(defn- ns-view [r]
  (let [{:keys [this ns-code-a]} r
        ns-eval @(:ns-eval-a this)]
    [:div {:style {:flex "1"
                   :min-height "0"
                   :display "flex"
                   :flex-direction "column"
                   :overflow "hidden"}}
     (when-let [ex (:ex ns-eval)]
       [:div {:style {:flex "0 0 auto"
                      :color "#b91c1c"
                      :background "#fef2f2"
                      :border "1px solid #fecaca"
                      :border-radius "0.25rem"
                      :padding "0.4rem 0.6rem"
                      :margin "0.5rem"
                      :font-size "0.8rem"
                      :font-family "monospace"
                      :white-space "pre-wrap"}}
        (ex-message ex)])
     [:div {:style {:flex "1" :min-height "0"}}
      (input-cm/codemirror-ui
       ns-code-a
       {:style {:height "100%"}
        :on-change (fn [code] (ns-code-change! r code))})]]))

(defn model-page
  "Form-3 view for one model.  Resolves the model name from the route, starts
   the runner on mount, and keeps it running while the ns/graph/interact tabs
   switch (the tab is server-side state, not separate views)."
  [ctx]
  (fn [req]
    (let [model-name (get-in req [:hyper/route :path-params :model-name])]
      (h/view
       {:mount
        (fn []
          (when-let [this (rui/start-model ctx model-name)]
            (rui/assign-missing-positions this)
            (let [base [:itera-model model-name]
                  tab-a (h/tab-cursor (conj base :tab) "graph")
                  ns-code-a (h/tab-cursor (conj base :ns-code)
                                          (or (rui/ns-data this) ""))]
              (h/watch! (:model-a this))
              (h/watch! (:index this))
              {:this this
               :tab-a tab-a
               :ns-code-a ns-code-a})))
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
                (tab-button r tab "ns")
                (tab-button r tab "graph")
                (tab-button r tab "interact")]
               (cond
                 (= "graph" tab)
                 [:div {:style {:flex "1"
                                :min-height "0"
                                :position "relative"
                                :display "flex"
                                :flex-direction "column"}}
                  (editor/graph-editor this)]

                 (= "ns" tab)
                 (ns-view r)

                 :else
                 (interact/interact-view r))])))
        :unmount (fn [r] (when r (rui/stop-model (:this r))))}))))