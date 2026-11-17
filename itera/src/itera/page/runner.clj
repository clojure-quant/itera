(ns itera.page.runner
  (:require [hyper.core :as h]
            [itera.runner.ui :as rui]))

(defn- hiccup-value? [v]
  (or (string? v)
      (and (vector? v) (keyword? (first v)))))

(defn- render-value [a]
  (if a
    (h/reactive [a]
                (let [v @a]
                  (when (hiccup-value? v) v)))
    [:p "unknown atom"]))

(defn runner-page
  "Form-3 view: starts the model named by the :model-name path-param on
   mount, streams each shown node's atom as hiccup, and stops it on unmount."
  [ctx]
  (fn [req]
    (let [model-name (get-in req [:hyper/route :path-params :model-name])]
      (h/view
       {:mount
        (fn []
          (let [this (rui/start-model ctx model-name)
                index-a (rui/index-a this)]
            (h/watch! index-a)
            #_(doseq [node-id (rui/currently-showing this)]
                (h/watch! (rui/get-atom this node-id)))
            {:this this 
             :index-a index-a}))
        :render
        (fn [{:keys [this index-a]} _req]
          [:div.page
           {:style {:max-width "1200px" :margin "0 auto" :padding "1rem"}}
           [:h1 (str "Runner: " model-name)]
           (for [node-id @index-a]
             (let [a (rui/get-atom this node-id)]
               [:div.node-row
                {:key node-id
                 :style {:display "flex" :gap "1rem"
                         :align-items "baseline"
                         :border-bottom "1px solid #e2e8f0"
                         :padding "0.25rem 0"}}
                [:div.node-id
                 {:style {:flex "0 0 12rem" :font-weight "600"
                          :font-family "monospace"}}
                 node-id]
                [:div.node-value (render-value a)]]))])
        :unmount (fn [{:keys [this index-a]}] (rui/stop-model this))}))))
