(ns itera.model-ui.interact
  "Interact tab: option-ui sidebar plus a live list of shown node values.

   Reads the model runner in `this` and its shown-node index cursor in `index`;
   option-ui nodes (value nodes with :value/editor :option-ui) are edited
   through itera.model.ui, and shown values render
   reactively via itera.value.converter.  Values implementing
   itera.plot-ui.protocol/itera-spec-plot render through that protocol at a
   fixed container size and expose an \"Edit spec\" CodeMirror popup; the edited
   spec is persisted on the node via itera.model.ui/set-node-spec."
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pprint]
            [hyper.core :as h]
            [itera.option.core :as options]
            [itera.plot-ui.protocol :as spec-plot]
            [itera.value.converter :as converter]
            [itera.model.ui :as rui]))

(def container-dimension
  "Fixed plot container size (px) for the interact tab."
  {:width 600 :height 400})

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
          (let [a     (rui/get-atom this node-id)
                label (or (:label (rui/node-by-id this node-id)) node-id)]
            [:div {:key node-id
                   :style {:border-bottom "1px solid #f1f5f9"
                           :padding "0.4rem 0"}}
             (if a
               (h/reactive [a]
                 (let [v     @a
                       spec? (satisfies? spec-plot/itera-spec-plot v)
                       spec  (rui/node-spec this node-id)]
                   [:div {:style {:display "flex"
                                  :flex-direction "column"
                                  :gap "0.35rem"}}
                    [:div {:style {:display "flex"
                                   :align-items "center"
                                   :gap "0.35rem"}}
                     (when spec?
                       [:button {:type "button"
                                 :title "Edit spec"
                                 :aria-label "Edit spec"
                                 :style {:cursor "pointer"
                                         :border "none"
                                         :background "transparent"
                                         :padding "0"
                                         :font-size "0.85rem"
                                         :line-height "1"}
                                 :data-on:click
                                 (h/action
                                  (reset! spec-code-a
                                          (with-out-str (pprint/pprint spec)))
                                  (reset! spec-popup-a node-id))}
                        "✏️"])
                     [:span {:style {:font-weight "600"
                                     :font-family "monospace"
                                     :font-size "0.8rem"}}
                      label]]
                    (if spec?
                      [:div {:style {:width (str (:width container-dimension) "px")}}
                       (spec-plot/to-formatted-hiccup v spec container-dimension)]
                      [:div {:style {:font-size "0.9rem" :min-width "0"}}
                       (converter/type->hiccup v)])]))
               [:div {:style {:display "flex" :flex-direction "column" :gap "0.35rem"}}
                [:div {:style {:font-weight "600" :font-family "monospace"
                               :font-size "0.8rem"}}
                 label]
                [:div {:style {:font-size "0.9rem"}}
                 [:span {:style {:color "#94a3b8"}} "—"]]])]))
        [:div {:style {:color "#64748b" :font-size "0.8rem"}} "No shown nodes."])]
     (when-let [node-id @spec-popup-a]
       (spec-popup this node-id spec-code-a spec-popup-a))]))
