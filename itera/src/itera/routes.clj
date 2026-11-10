(ns itera.routes
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [itera.discover :as discover]))

(defn- split-name
  "Split an alg name \"<ns>/<symbol>\" into {:ns <ns> :symbol <symbol>}."
  [name]
  (let [[n s] (str/split name #"/" 2)]
    {:ns n :symbol s}))

(defn- overview-page
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

(defn- algo-page
  [req]
  (let [{n :ns s :symbol} (get-in req [:hyper/route :path-params])]
    [:div.page [:h1 (str n "/" s)]]))

(defn routes
  "Build the itera routes. Calls discover-start once and returns
   {:routes <vector> :discover <discover-start state>}."
  [ctx]
  (let [d (discover/discover-start)
        result* (:result d)]
    {:routes
     [["/itera" {:name :itera
                 :title "Itera"
                 :get (overview-page result*)}]
      ["/itera/:ns/:symbol" {:name :itera-algo
                             :title "Algo"
                             :get #'algo-page}]]
     :discover d}))
