(ns itera.routes
  (:require [itera.page.algo :as page-algo]
            [itera.page.overview :as page-overview]))

(defn routes
  "Build the itera routes. Takes the application ctx and the discovery state
   returned by itera.algo.discover/discover-start, and returns the routes vector."
  [ctx discover]
  (let [result* (:result discover)]
    [["/itera" {:name :itera
                :title "Itera"
                :get (page-overview/overview-page result*)}]
     ["/itera/algo/:ns/:symbol" {:name  :itera-algo
                                 :title "Algo"
                                 :get   (page-algo/algo-page ctx result*)}]]))