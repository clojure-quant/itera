(ns itera.model-ui.interact
  "Interact tab: option-ui sidebar plus a live list of shown node values.

   Reads the model runner in `this` and its shown-node index cursor in `index`;
   option-ui nodes are edited through itera.runner.ui, and shown values render
   reactively via itera.model-ui.visual-value."
  (:require [itera.model-ui.visual-value :as visual]
            [itera.option.core :as options]
            [itera.runner.ui :as rui]))

(defn interact-view [r]
  (let [{:keys [this]} r
        index-a (:index this)
        option-nodes (rui/option-ui-nodes this)]
    [:div {:style {:flex "1" :min-height "0" :display "flex"}}
     [:div {:class "model-sidebar"
            :style {:flex "0 0 300px"
                    :max-width "300px"
                    :min-width "0"
                    :overflow-y "auto"
                    :border-right "1px solid #e2e8f0"
                    :padding "0.75rem"}}
      (if (seq option-nodes)
        (for [{:keys [node-id label spec value]} option-nodes]
          (let [base (if (map? value) value {})]
            [:div {:key node-id :style {:margin-bottom "1.25rem"}}
             [:div {:style {:font-weight "600"
                            :font-size "0.8rem"
                            :margin-bottom "0.35rem"
                            :font-family "monospace"}}
              (or label node-id)]
             (options/options-ui
              {:group (keyword "model" node-id)
               :spec spec
               :value value
               :set-fn (fn [p v] (rui/set-node-value this node-id (assoc-in base p v)))
               :update-fn (fn [p f] (rui/set-node-value this node-id (update-in base p f)))})]))
        [:div {:style {:color "#64748b" :font-size "0.8rem"}}
         "No value nodes with an option-ui spec."])]
     [:div {:class "model-values"
            :style {:flex "1"
                    :min-width "0"
                    :overflow-y "auto"
                    :padding "0.75rem"}}
      (if (seq @index-a)
        (for [node-id (sort @index-a)]
          (let [a (rui/get-atom this node-id)
                label (or (:label (rui/node-by-id this node-id)) node-id)]
            [:div {:key node-id
                   :style {:display "flex"
                           :gap "1rem"
                           :align-items "baseline"
                           :border-bottom "1px solid #f1f5f9"
                           :padding "0.3rem 0"}}
             [:div {:style {:flex "0 0 12rem"
                            :font-weight "600"
                            :font-family "monospace"
                            :font-size "0.8rem"}}
              label]
             [:div {:style {:font-size "0.9rem"}} (visual/render-value a)]]))
        [:div {:style {:color "#64748b" :font-size "0.8rem"}} "No shown nodes."])]]))