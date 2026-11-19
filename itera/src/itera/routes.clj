(ns itera.routes
  (:require [itera.model-ui.image-cache :as image-cache]
            [itera.model-ui.page-model :as page-model]
            [itera.model-ui.page-overview :as page-model-overview]
            [itera.page.algo :as page-algo]
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
     ["/itera/image-cache/:image-id" {:name :itera-image-cache
                                      :title "Image cache"
                                      :hyper/disabled? true
                                      :get image-cache/image-handler}]
     ["/itera/model/:model-name" {:name  :itera-model
                                  :title "Model"
                                  :get   (page-model/model-page ctx)}]]))
