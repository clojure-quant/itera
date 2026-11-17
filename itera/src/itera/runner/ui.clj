(ns itera.runner.ui
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as string]
   [missionary.core :as m]
   [itera.runner.core :as rc])
  (:import
   [java.io File]))


(defn- model-path [ctx]
  (or (:itera/model-path ctx)
      (throw (ex-info "missing :itera/model-path in ctx" {:ctx ctx}))))

(defn available-models [ctx]
  (->> (.listFiles (io/file (model-path ctx)))
       (filter #(and (.isFile ^File %)
                     (string/ends-with? (.getName ^File %) ".edn")))
       (map #(string/replace (.getName ^File %) #"\.edn$" ""))
       (sort)
       (into [])))

(defn load-model [ctx model-name]
  (edn/read-string (slurp (io/file (model-path ctx) (str model-name ".edn")))))


(defn show? [{:keys [:node-dict-a]}]
  (->> @node-dict-a
       (filter (fn [[id {:keys [show?]
                         :or {show? false}}]]
                 show?))
       (into {})))

(defn watch-cell [{:keys [runner showing disposers index]} node-id]
  (let [node (rc/get-node runner node-id)
        a (atom nil)
        r (m/reduce (fn [_ v]
                      ;(println "r: " v)
                      (reset! a v)
                      nil) nil (:out node))
        dispose! (r #(pr-str "started " node-id %)
                    #(pr-str "stoped " node-id %))]
    (swap! index conj node-id)
    (swap! showing assoc node-id a)
    (swap! disposers assoc node-id dispose!)))

(defn unwatch-cell [{:keys [runner showing disposers index]} node-id]
  (when (contains? @showing node-id)
    (let [d! (get @disposers node-id)]
      (d!)
      (swap! index disj node-id)
      (swap! showing dissoc node-id)
      (swap! disposers dissoc node-id))))


(defn start-model [ctx model-name]
  (let [m (load-model ctx model-name)
        r (rc/create-runner ctx m)
        this {:runner r
              :showing (atom {})
              :disposers (atom {})
              :index (atom #{})
              }
        start-nodes (-> r show? keys)
        ]
    (doall (map #(watch-cell this %) start-nodes))
    this))

(defn index-a [{:keys [index]}]
  index)


(defn currently-showing [{:keys [showing]}]
  (keys @showing))

(defn stop-model [this]
  (let [nodes-running (currently-showing this)]
    (doall (map #(unwatch-cell this %) nodes-running))
    ))



(defn get-atom [{:keys [showing]} node-id]
  (get @showing node-id))

(comment
  (def ctx {:itera/model-path "../models"})
  (available-models ctx)
  (load-model ctx "sum")
  (def x (start-model ctx "sum_ui"))
  (currently-showing x)
  (get-atom x "sum-ui") 
  (stop-model x)

  (-> #{"a"}
      (conj  "B")
      (disj "a"))


 ; 
  )

