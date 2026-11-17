(ns itera.routes
  (:require [itera.page.algo :as page-algo]
            [itera.page.graph :as page-graph]
            [itera.page.overview :as page-overview]
            [itera.page.runner :as page-runner]))

(defn routes
  "Build the itera routes. Takes the application ctx and the discovery state
   returned by itera.algo.discover/discover-start, and returns the routes vector."
  [ctx discover]
  (let [result* (:result discover)]
    [["/itera" {:name :itera
                :title "Itera"
                :get (page-overview/overview-page result*)}]
     ["/itera/graph" {:name :itera-graph
                      :title "Graph"
                      :get (page-graph/graph-page ctx)}]
     ["/itera/algo/:ns/:symbol" {:name  :itera-algo
                                 :title "Algo"
                                 :get   (page-algo/algo-page ctx result*)}]
     ["/itera/run/:model-name" {:name  :itera-runner
                                :title "Runner"
                                :get   (page-runner/runner-page ctx)}]]))