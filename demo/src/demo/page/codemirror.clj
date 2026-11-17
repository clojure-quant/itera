(ns demo.page.codemirror
  "CodeMirror demo: edit Clojure on the left, see the bound cursor value on the
   right."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-codemirror]))

(def initial-code
  "(ns demo
  (:require [clojure.string :as str]))

(defn greet [name]
  (str \"Hello, \" name \"!\"))

(comment
  (greet \"world\"))
")

(defn page-codemirror
  "Form-2 view.  Left: itera.input.codemirror/codemirror-ui over a tab cursor
   seeded initial-code.  Right: (pr-str @code-a) as text."
  [_ctx]
  (fn [_req]
    (let [code-a (h/tab-cursor [:codemirror-demo :code] initial-code)]
      [:div.page {:style {:padding "1rem"}}
       [:h1 "CodeMirror demo"]
       [:div {:style {:display "flex" :gap "1rem" :align-items "stretch"}}
        [:div {:style {:flex "1 1 50%" :min-width "0" :height "480px"}}
         (input-codemirror/codemirror-ui code-a {:style {:height "100%"}})]
        [:div {:style {:flex "1 1 50%" :min-width "0"
                       :display "flex" :flex-direction "column" :gap "0.5rem"}}
         [:div {:style {:font-size "0.8rem" :color "#6b7280"}}
          "(pr-str @code-a) — updates when the editor blurs"]
         [:pre {:style {:margin 0 :padding "0.75rem" :background "#f6f6f6"
                        :border-radius "0.375rem" :overflow "auto" :flex "1"}}
          (pr-str @code-a)]]]])))
