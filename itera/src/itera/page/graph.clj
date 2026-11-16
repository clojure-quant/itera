(ns itera.page.graph
  "Graph editor page.

   Owns the global graph cursor [:itera-graph :graph] and the last-save error
   cursor [:itera-graph :last-errors].  The client component edits the graph
   and emits a save event; this page validates the payload server-side through
   itera.graph/save-graph! and re-renders from the cursor."
  (:require [hyper.core :as h]
            [itera.graph :as graph]
            [itera.graph.editor :as editor]))

(defn graph-page
  "Form-3 view for the graph editor.  Resolves the node-type catalog from
   (:graph :node-types) in the application ctx and seeds the graph cursor with
   (:graph :initial-graph) on first mount."
  [ctx]
  (let [catalog (graph/node-catalog (get-in ctx [:graph :node-types]))
        initial (graph/normalize-graph
                 catalog
                 (or (get-in ctx [:graph :initial-graph]) graph/empty-graph))]
    (fn [_req]
      (h/view
       {:mount
        (fn []
          (let [graph-a (h/global-cursor [:itera-graph :graph] initial)
                errors-a (h/global-cursor [:itera-graph :last-errors])
                save-a (h/global-cursor [:itera-graph :last-save])]
            (reset! errors-a nil)
            (reset! save-a nil)
            {:catalog catalog
             :graph-a graph-a
             :errors-a errors-a
             :save-a save-a}))
        :render
        (fn [r _req]
          (let [types (:types (:catalog r))
                current (graph/normalize-graph (:catalog r)
                                               (or @(:graph-a r) graph/empty-graph))
                errors @(:errors-a r)
                save-result @(:save-a r)]
            [:div.page {:style {:padding "1rem"}}
             [:div {:style {:display "flex" :align-items "center"
                            :justify-content "space-between"
                            :gap "1rem" :margin-bottom "0.5rem"}}
              [:h1 {:style {:margin 0 :font-size "1.35rem"}} "Graph editor"]
              [:a (merge {:style {:font-weight 600}}
                         (h/navigate :itera))
               "← Algos"]]
             (if (empty? types)
               [:div {:style {:padding "1rem" :border "1px solid #fecaca"
                              :background "#fef2f2" :color "#991b1b"
                              :border-radius "6px"}}
                "No graph node catalog configured. Pass :graph {:node-types [...]} in the application ctx."]
               [:div
                [:div {:style (merge {:margin-bottom "0.5rem" :padding "0.5rem 0.75rem"
                                      :border "1px solid #fecaca" :background "#fef2f2"
                                      :color "#991b1b" :border-radius "6px"
                                      :font-size "0.8rem"}
                                     (when-not (seq errors) {:display "none"}))}
                 (when (seq errors)
                   [:div
                    [:strong {:style {:display "block" :margin-bottom "0.2rem"}}
                     "Save failed; the stored graph was kept."]
                    [:ul {:style {:margin 0 :padding-left "1.1rem"}}
                     (for [{:keys [code message]} errors]
                       [:li {:key (str code "-" message)} message])]])]
                (editor/itera-graph-editor
                 {:graph current
                  :node-types types
                  :save-result save-result
                  :style {:display "block"
                          :height "calc(100vh - 150px)"
                          :min-height "480px"}
                  :data-on:save
                  (h/action
                   (when-let [g (get $detail :graph)]
                     (let [result (graph/save-graph! (:catalog r) (:graph-a r) g)]
                       (reset! (:errors-a r)
                               (when-not (:ok? result) (:errors result)))
                       (reset! (:save-a r)
                               {:status (if (:ok? result) "ok" "error")}))))})
                [:div {:style {:margin-top "0.4rem" :font-size "0.7rem" :color "#64748b"}}
                 "Typed ports, one edge per input, no cycles. Save stores the graph in server memory."]])]))
        :unmount (fn [_] nil)}))))