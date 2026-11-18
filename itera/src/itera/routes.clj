(ns itera.routes
  (:require [itera.page.algo :as page-algo]
            [itera.page.model :as page-model]
            [itera.page.model-overview :as page-model-overview]
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
                                 :get   (page-algo/algo-page ctx result*)}]
     ["/itera/model" {:name :itera-model-list
                      :title "Models"
                      :get (page-model-overview/model-overview-page ctx)}]
     ["/itera/model/:model-name" {:name  :itera-model
                                  :title "Model"
                                  :get   (page-model/model-page ctx)}]]))
