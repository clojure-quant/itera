(ns itera.routes
  (:require [itera.model-ui.image-cache :as image-cache]
            [itera.model-ui.page-model :as page-model]
            [itera.model-ui.page-overview :as page-model-overview]))

(defn routes
  "Build the itera routes for the application ctx."
  [ctx]
  [["/itera" {:name :itera
              :title "Itera"
              :get (page-model-overview/model-overview-page ctx)}]
   ["/itera/model" {:name :itera-model-list
                    :title "Models"
                    :get (page-model-overview/model-overview-page ctx)}]
   ["/itera/image-cache/:image-id" {:name :itera-image-cache
                                    :title "Image cache"
                                    :hyper/disabled? true
                                    :get image-cache/image-handler}]
   ["/itera/model/:model-name" {:name :itera-model
                                :title "Model"
                                :get (page-model/model-page ctx)}]])
