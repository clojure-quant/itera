(ns demo.page.option
  "Options playground: author a spec on the left, see the rendered
   options-ui and the live value on the right."
  (:require [hyper.core :as h]
            [itera.input.option :as input-option]
            [itera.option.core :as options]
            [itera.util.clj :as util]))

(def predefined
  [{:name "animal keywords"
    :rows [{:path [:secret-animal] :type :select :spec "[:bird :elephant :lion :eagle :ant]"}]}
   {:name "colors (from namespace)"
    :rows [{:path [:secret-number] :type :select :spec "(require '[itera.util.color :refer [colors]])
   (keys colors)"}]}
   ])

(defn page-option-playground
  "Form-2 view.  Left: itera.input.option/option-input over a tab cursor
   seeded [].  Right: options-ui built from create-option-spec of that cursor,
   over a value cursor seeded {}, plus (pr-str value)."
  [ctx]
  (let [workbook "demo.options.playground"
        _        (util/create-workbook workbook (or (:requires ctx) []))]
    (fn [_req]
      (let [options-a (h/tab-cursor [:option-playground :options] [])
            value-a   (h/tab-cursor [:option-playground :value] {})]
        [:div.page {:style {:padding "1rem"}}
         [:h1 "Options playground"]
         [:div {:style {:display "flex" :gap "1rem" :align-items "flex-start"}}
          [:div {:style {:flex "1 1 50%" :min-width "0"}}
           (input-option/option-input options-a predefined)]
          [:div {:style {:flex "1 1 50%" :min-width "0"
                         :display "flex" :flex-direction "column" :gap "1rem"}}
           [:div
            (options/options-ui
             {:group :playground
              :spec  (input-option/create-option-spec workbook @options-a)
              :value @value-a
              :set-fn    (fn [p v] (swap! value-a assoc-in p v))
              :update-fn (fn [p f] (swap! value-a update-in p f))})]
           [:pre {:style {:margin 0 :padding "0.75rem" :background "#f6f6f6"
                          :border-radius "0.375rem" :overflow "auto"}}
            (pr-str @value-a)]]]]))))
