(ns itera.runner.core
  (:require
   [missionary.core :as m]
   [itera.util.clj :as util]
   [itera.runner.formula :as f]))

(defn node-dict [{:keys [nodes edges] :as persistent-data}]
  (->> nodes
       (map (juxt :id identity))
       (into {})))

(defn add-node! [{:keys [node-dict-a]} node]
  (swap! node-dict-a assoc (:id node) node))

(defn get-node [{:keys [node-dict-a]} node-id]
  (get @node-dict-a node-id))

(defn create-value-node [{:keys [id value] :as persistent}]
  (let [value-a (atom value)
        out (m/signal (m/watch value-a))]
    (assoc persistent
           :value-a value-a
           :out out)))

(defn inputs-for-formula [edges formula-id]
  (->> edges
       (filter #(= formula-id (:target-node %)))
       (map (juxt :target-port :source-node))
       (into {})))

; no value

(defrecord no-val [cell-id])

(defn create-no-val [cell-id]
  (no-val. cell-id))

(defn is-no-val? [v]
  (instance? no-val v))

(defn clj-calculate [ctx f input-flows]
  (let [input-f (apply m/latest vector input-flows)
        formula-result-f (m/ap
                          (m/amb (create-no-val "init"))
                          (let [args-vec (seq (m/?> input-f))]
                            (if (some is-no-val? args-vec)
                              (create-no-val "input init")
                              (try
                                (m/? (m/via m/cpu (f/run-formula ctx f args-vec)))
                                (catch Exception ex
                                  (create-no-val (str "ex: " (ex-message ex))))))))
        formula-result-f-wrapped (->> formula-result-f
                                      (m/reductions (fn [r v] v) (create-no-val "init2"))
                                      (m/relieve {})
                                      (m/signal))]
    formula-result-f-wrapped))

(defn create-formula-node [{:keys [ctx graph-ns edges-a] :as r} {:keys [id code] :as persistent}]
  (let [input-dict (inputs-for-formula @edges-a id)
        input-flow-dict (->> input-dict
                             (map (fn [[arg node-id]]
                                    [arg (:out (get-node r node-id))]))
                             (into {}))
        {:keys [fn args ctx? value flow?] :as formula} (-> (util/compile-formula graph-ns code)
                                                           (f/create-formula))
        args-vec (f/args-val formula input-flow-dict)
        persistent (assoc persistent :args (vec (or args [])) :formula formula)]
    (cond
      ; a value that is a flow
      (and flow? value) (assoc persistent
                               :out value)

      ; a function that does not compile (or is not a function)
      (nil? fn) persistent

      ; not all inputs are connected
      (nil? args-vec) persistent
      :else  (assoc persistent
                    :out (clj-calculate ctx formula args-vec)))))

(defn update-formula-node!
  "Recompute a formula node in place (the same :node-dict-a key), mirroring
   create-formula-node's rules so an existing node is never duplicated.  Uses
   the node's current :code and the current :edges-a / upstream :out flows."
  [r node-id]
  (when-let [node (get-node r node-id)]
    (let [updated (create-formula-node r node)]
      (swap! (:node-dict-a r) assoc node-id updated)
      updated)))

(defn create-runner [ctx {:keys [nodes edges] :as persistent-data}]
  (let [graph-ns (str "itera.user." (:graph/name persistent-data))
        _ (util/create-workbook graph-ns (or (:requires ctx) []))
        this  {:graph-ns graph-ns
               :ctx ctx
               :node-dict-a (atom {})
               :edges-a (atom edges)}
        value-nodes (->> nodes
                         (filter #(= :value (:type %))))
        _ (doall (map (comp  #(add-node! this %) create-value-node) value-nodes))
        formula-nodes (->> nodes
                           (filter #(= :formula (:type %))))
        _ (doall (map (comp  #(add-node! this %) #(create-formula-node this %)) formula-nodes))]
    this))



(defn reset-input! [runner node-id new-value]
  (if-let [node (get-node runner node-id)]
    (reset! (:value-a node) new-value)
    (throw (ex-info (str "cannot reset non-existing input cell" node-id) {:node-id node-id}))))

(defn swap-input! [runner node-id f & args]
  (if-let [node (get-node runner node-id)]
    (apply swap! (:value-a node) f args)
    (throw (ex-info (str "cannot swap non-existing input cell" node-id) {:node-id node-id}))))

(defn- take-first-val [f]
  ; flows dont implement deref
  (m/eduction
   (remove is-no-val?)
   (take 1)
   f))

(defn- current-v
  "gets the first valid value from the flow"
  [f]
  (m/reduce (fn [r v]
              ;(println "current v: " v " r: " r)
              v) nil
            (take-first-val f)))

(defn get-snapshot [r id]
  (-> (get-node r id)
      :out
      (current-v)
      (m/?)))


(comment

  (inputs-for-formula [{:id "e-1" :source-node "a-node" :source-port "out"
                        :target-node "sum" :target-port "a"}
                       {:id "e-2" :source-node "b-node" :source-port "out"
                        :target-node "sum" :target-port "b"}] "sum")


   ; 
  )