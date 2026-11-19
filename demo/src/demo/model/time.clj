(ns demo.model.time
  (:require
   [itera.model.ui :as ui]))

(def ctx {:itera/model-path "../models"
          :requires '[[clojure.string :as string]
                      [clojure.edn :as edn]
                      [demo.flow :as df]]})

(def r (ui/start-model ctx "time"))


(ui/get-atom r "gCXaw4JYslAskzOB")

(ui/get-atom r "GZJoZ0bizloz46-b") 
(ui/get-atom r "TKrK2syZ0a0zVc1g")


(ui/stop-model r)