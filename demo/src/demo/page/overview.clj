(ns demo.page.overview
  "Overview of the demo server: links to every /demo/xxx page."
  (:require [clojure.string :as str]
            [hyper.core :as h]))

(defn demo-overview-page
  "Form-2 view.  Renders an index of links to `pages`, a coll of
   {:name <route-name-kw> :title <str> :path <str>}."
  [pages]
  (fn [_req]
    (let [pages (sort-by (fn [p] (str/lower-case (str (:title p)))) pages)]
      [:div.page {:style {:padding "1rem"}}
       [:h1 "Demo"]
       [:ul {:style {:list-style-type "none" :padding-left "0"}}
        (for [{:keys [title] nm :name} pages]
          [:li {:key nm :style {:margin "0.25rem 0"}}
           [:a (merge {:style {:font-weight "600"}} (h/navigate nm))
            (or title (name nm))]])]])))
