(ns demo.runner.sum
  (:require
   [itera.runner.core :as rc]))

(def graph
  {:graph/name "sum"
   :nodes [{:id "a-node" :type :value
            :position {:x 60 :y 120}
            :value 15}
           {:id "b-node" :type :value
            :position {:x 90 :y 120}
            :value 5}
           {:id "sum" :type :formula
            :position {:x 120 :y 120}
            :code "(fn [_ a b] (+ a b))"}]
   :edges [{:id "e-1" :source-node "a-node" :source-port "out"
            :target-node "sum" :target-port "a"}
           {:id "e-2" :source-node "b-node" :source-port "out"
            :target-node "sum" :target-port "b"}]})

(def ctx {})

(def r (rc/create-runner ctx graph))

(rc/get-snapshot r "sum")

(rc/reset-input! r "a-node" 100)
(rc/get-snapshot r "sum")

(rc/reset-input! r "b-node" -40)
(rc/get-snapshot r "sum")


