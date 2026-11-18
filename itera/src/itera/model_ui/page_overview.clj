(ns itera.page.model-overview
  "List the model files under :itera/model-path, linking to the model page."
  (:require [hyper.core :as h]
            [itera.runner.ui :as rui]))

(defn model-overview-page
  "Form-1 view listing the available models."
  [ctx]
  (fn [_req]
    (let [models (rui/available-models ctx)]
      [:div.page
       [:div {:style {:display "flex" :align-items "baseline" :gap "1rem"}}
        [:h1 "Models"]
        [:a (merge {:style {:font-weight "600"}} (h/navigate :itera)) "← Itera"]]
       (if (seq models)
         [:ul {:style {:list-style-type "none" :padding-left "0"}}
          (for [m models]
            [:li {:key m}
             [:a (merge {:style {:font-family "monospace"}}
                        (h/navigate :itera-model {:model-name m}))
              m]])]
         [:div {:style {:color "#64748b"}} "No models found."])])))
