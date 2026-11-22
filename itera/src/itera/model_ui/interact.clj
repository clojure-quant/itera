(ns itera.model-ui.interact
  "Interact tab: option-ui sidebar plus a live list of shown node values.

   Reads the model runner in `this` and its shown-node index cursor in `index`;
   option-ui nodes (value nodes with :value/editor :option-ui) are edited
   through itera.model.ui.  Each shown value renders reactively through
   itera.model-ui.cell-ui/cell-ui; values implementing
   itera.value.protocol/itera-spec-plot expose an \"Edit spec\" CodeMirror
   popup rendered here, and the edited spec is persisted on the node via
   itera.model.ui/set-node-spec."
  (:require [clojure.edn :as edn]
            [hyper.core :as h]
            [itera.option.core :as options]
            [itera.value.protocol :as spec-plot]
            [itera.model-ui.cell-ui :as cell-ui]
            [itera.model.ui :as rui]))

(defn- close-spec!
  "Read the popup editor text as EDN and persist it onto the node's :spec,
   then close the popup.  Bad EDN leaves the spec unchanged."
  [this node-id code-a popup-a]
  (try (rui/set-node-spec this node-id (edn/read-string @code-a))
       (catch Exception _ nil))
  (reset! popup-a nil))

(defn- spec-popup [this node-id code-a popup-a]
  (let [a (rui/get-atom this node-id)
        v (when a @a)
        spec (rui/node-spec this node-id)]
    [:div {:style {:position "fixed"
                   :inset "0"
                   :background "rgba(17,24,39,0.45)"
                   :z-index "1000"
                   :display "flex"
                   :align-items "center"
                   :justify-content "center"
                   :padding "1rem"}
           :data-on:click (h/action (close-spec! this node-id code-a popup-a))}
     [:div {:style {:background "white"
                    :border-radius "0.5rem"
                    :box-shadow "0 10px 30px rgba(0,0,0,0.2)"
                    :width "min(820px,100%)"
                    :max-height "92vh"
                    :display "flex"
                    :flex-direction "column"
                    :gap "0.6rem"
                    :padding "0.85rem"}
            :data-on:click (h/expr (.stopPropagation evt))}
      [:div {:style {:font-weight "600"
                     :font-size "0.85rem"
                     :font-family "monospace"}}
       "Edit plot spec"]
      (when v (spec-plot/spec-editor v spec code-a))
      [:div {:style {:display "flex" :justify-content "flex-end"}}
       [:button {:type "button"
                 :data-on:click (h/action (close-spec! this node-id code-a popup-a))}
        "Close"]]]]))

(defn interact-view [r]
  (let [{:keys [this]} r
        index-a (:index this)
        option-nodes (rui/option-ui-nodes this)
        base         [:itera-model (:model-name this) :interact]
        spec-popup-a (h/tab-cursor (conj base :spec-popup) nil)
        spec-code-a  (h/tab-cursor (conj base :spec-code) "")]
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
          (cell-ui/cell-ui this node-id))
        [:div {:style {:color "#64748b" :font-size "0.8rem"}} "No shown nodes."])]
     (when-let [node-id @spec-popup-a]
       (spec-popup this node-id spec-code-a spec-popup-a))]))
