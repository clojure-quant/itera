(ns itera.model-ui.page-overview
  "List the model files under :itera/model-path, linking to the model page,
   and create new empty models."
  (:require [hyper.core :as h]
            [itera.model.ui :as rui]))

(defn model-overview-page
  "Form-2 view listing the available models and a form to add one."
  [ctx]
  (fn [_req]
    (let [name* (h/signal [:itera-model-list :new-name] "")
          status-a (h/tab-cursor [:itera-model-list :status] nil)]
      (fn [_req]
        (let [models (rui/available-models ctx)
              status @status-a]
          [:div.page
           [:div {:style {:display "flex" :align-items "baseline" :gap "1rem"}}
            [:h1 "Models"]
            [:a (merge {:style {:font-weight "600"}} (h/navigate :itera)) "← Itera"]]
           [:form {:data-on:submit__prevent
                   (h/action
                    (let [{:keys [ok? name error]} (rui/create-model! ctx @name*)]
                      (reset! status-a (if ok?
                                         {:ok? true :name name}
                                         {:ok? false :error error}))
                      (when ok? (reset! name* ""))))
                   :style {:display "flex"
                           :align-items "center"
                           :gap "0.5rem"
                           :margin "1rem 0"}}
            [:input {:type "text"
                     :name "name"
                     :data-bind name*
                     :placeholder "New model name"
                     :style {:font-size "0.8rem"
                             :padding "0.3rem 0.5rem"
                             :border "1px solid #cbd5e1"
                             :border-radius "4px"
                             :color "#334155"
                             :min-width "14rem"}}]
            [:button {:type "submit"
                      :style {:font-size "0.8rem"
                              :padding "0.3rem 0.75rem"
                              :border "1px solid #cbd5e1"
                              :background "#f8fafc"
                              :border-radius "4px"
                              :cursor "pointer"
                              :color "#334155"}}
             "Add model"]]
           (when status
             (if (:ok? status)
               [:p {:role "status"
                    :style {:color "#15803d" :font-size "0.85rem" :margin "0 0 1rem"}}
                (str "Created model \"" (:name status) "\".")]
               [:p {:role "alert"
                    :style {:color "#b91c1c" :font-size "0.85rem" :margin "0 0 1rem"}}
                (:error status)]))
           (if (seq models)
             [:ul {:style {:list-style-type "none" :padding-left "0"}}
              (for [m models]
                [:li {:key m}
                 [:a (merge {:style {:font-family "monospace"}}
                            (h/navigate :itera-model {:model-name m}))
                  m]])]
             [:div {:style {:color "#64748b"}} "No models found."])])))))
