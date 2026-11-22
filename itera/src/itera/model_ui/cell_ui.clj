(ns itera.model-ui.cell-ui
  "Render one shown model node as an interact-tab cell.

   Owns the spec-edit button and the private cursors it seeds; the popup that
   edits the spec is rendered by itera.model-ui.interact, which shares the same
   cursor paths."
  (:require [clojure.pprint :as pprint]
            [hyper.core :as h]
            [itera.value.protocol :as spec-plot]
            [itera.value.converter :as converter]
            [itera.model.ui :as rui]))

(def container-dimension
  "Fixed plot container size (px) for the interact tab."
  {:width 600 :height 400})

(defn- spec-cursors
  "Spec-popup cursors for the cell's edit button.  The path is shared with
   interact-view, which renders the modal for the open node id."
  [this]
  (let [base [:itera-model (:model-name this) :interact]]
    {:popup-a (h/tab-cursor (conj base :spec-popup) nil)
     :code-a  (h/tab-cursor (conj base :spec-code) "")}))

(defn- page-cursor
  "Page cursor for a cell value that implements itera-page-plot.  The default
   is init-page's result; tab-cursor applies it only when the path was never
   written."
  [this cell-id v spec]
  (h/tab-cursor (conj [:itera-model (:model-name this) :interact :page]
                      cell-id)
                (spec-plot/init-page v spec container-dimension)))

(defn cell-ui
  "Render one shown model node as a keyed cell.

   cell-id is the model node id.  A spec-plot value gets an edit button that
   seeds the shared spec popup cursors; other values go through the converter.
   A spec-plot value that also implements itera-page-plot renders one page at
   a time and gets the page editor below the plot, constrained to the plot
   container width.  A node with no live atom renders its label and an em dash."
  [this cell-id]
  (let [{:keys [popup-a code-a]} (spec-cursors this)
        a     (rui/get-atom this cell-id)
        label (or (:label (rui/node-by-id this cell-id)) cell-id)
        v0     (when a @a)
        spec   (rui/node-spec this cell-id)
        page?  (and v0
                    (satisfies? spec-plot/itera-spec-plot v0)
                    (satisfies? spec-plot/itera-page-plot v0))
        page-a (when page? (page-cursor this cell-id v0 spec))
        deps   (cond-> [a popup-a code-a] page-a (conj page-a))]
    [:div {:key cell-id
           :style {:border-bottom "1px solid #f1f5f9"
                   :padding "0.4rem 0"}}
     (if a
       (h/reactive deps
                   (let [v      @a
                         spec?  (satisfies? spec-plot/itera-spec-plot v)
                         spec   (rui/node-spec this cell-id)
                         page?  (and v
                                     (satisfies? spec-plot/itera-spec-plot v)
                                     (satisfies? spec-plot/itera-page-plot v))
                         page-a (or page-a
                                    (when page? (page-cursor this cell-id v spec)))]
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
                                    (reset! code-a
                                            (with-out-str (pprint/pprint spec)))
                                    (reset! popup-a cell-id))}
                          "✏️"])
                       [:span {:style {:font-weight "600"
                                       :font-family "monospace"
                                       :font-size "0.8rem"}}
                        label]]

                      (if page?
                        [:div {:style {:display "flex"
                                       :flex-direction "column"
                                       :gap "0.35rem"}}
                         [:div {:style {:min-width "0"
                                        :min-height "0"
                                        :max-width (str (:width container-dimension) "px")
                                        :max-height (str (:height container-dimension) "px")
                                        :overflow "auto"
                                        :border "dotted"
                                        :border-color "red"}}
                          (spec-plot/to-formatted-paged-hiccup
                           v spec container-dimension @page-a)]
                          [:div {:style {:max-width (str (:width container-dimension) "px")}}
                           (spec-plot/page-editor v spec container-dimension page-a)]]
                        [:div {:style {:min-width "0"
                                       :min-height "0"
                                       :max-width (str (:width container-dimension) "px")
                                       :max-height (str (:height container-dimension) "px")
                                       :overflow "auto"
                                       :border "dotted"
                                       :border-color "red"}}
                         (if spec?
                           (spec-plot/to-formatted-hiccup v spec container-dimension)
                           (converter/type->hiccup v))])]))

       ;; no value
       [:div {:style {:display "flex" :flex-direction "column" :gap "0.35rem"}}
        [:div {:style {:font-weight "600" :font-family "monospace"
                       :font-size "0.8rem"}}
         label]
        [:div {:style {:font-size "0.9rem"}}
         [:span {:style {:color "#94a3b8"}} "—"]]])]))