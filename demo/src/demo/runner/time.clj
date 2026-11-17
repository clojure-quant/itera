(ns demo.runner.time
  (:require
   [itera.runner.ui :as ui]))

(def ctx {:itera/model-path "../models"
          :requires '[[clojure.string :as string]
                      [clojure.edn :as edn]
                      [demo.flow :as df]]})

(def r (ui/start-model ctx "time"))


(ui/get-atom r "time-ui")

(ui/get-atom r "est-ui") 
(ui/get-atom r "local-ui")


(ui/stop-model r)