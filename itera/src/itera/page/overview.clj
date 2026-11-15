(ns itera.page.overview
  (:require [clojure.string :as str]
            [hyper.core :as h]))

(defn- split-name
  "Split an alg name \"<ns>/<symbol>\" into {:ns <ns> :symbol <symbol>}."
  [name]
  (let [[n s] (str/split name #"/" 2)]
    {:ns n :symbol s}))

(defn overview-page
  "Form-2 view: subscribes to the discover result atom so the page re-renders
   when itera.clj files change."
  [result*]
  (fn [req]
    (h/watch! result*)
    (fn [req]
      (let [{:keys [algos error]} @result*]
        [:div.page
         [:h1 "Itera"]
         [:ul
          (for [{:keys [name]} algos]
            [:li {:key name}
             [:a (h/navigate :itera-algo (split-name name)) name]])]
         (when (seq error)
           [:div.errors
            [:h2 "Errors"]
            [:table
             [:thead [:tr [:th "file"] [:th "error"]]]
             [:tbody
              (for [{:keys [file error]} error]
                [:tr {:key file} [:td file] [:td (str error)]])]]])]))))
