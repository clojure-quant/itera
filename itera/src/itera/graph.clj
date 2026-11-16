(ns itera.graph
  "Canonical node-graph model for the graph editor.

   A graph is {:nodes [...] :edges [...]}.  Each node is
     {:id \"n-1\" :type :sma :position {:x 80 :y 120} :fields {:window 20}}
   and each edge is
     {:id \"e-1\" :source-node \"n-1\" :source-port \"out\"
      :target-node \"n-2\" :target-port \"data\"}.

   Node types are supplied by the application ctx as a vector of
     {:type :sma
      :label \"SMA\"
      :inputs  [{:id \"data\" :label \"Data\" :data-type :dataset}]
      :outputs [{:id \"out\"  :label \"SMA\"  :data-type :dataset}]
      :fields  [{:key :window :label \"Window\" :type :number :default 20}]}
   maps.  normalize-node-types returns the normalized catalog as both a
   vector and a lookup map.

   Everything here is pure except save-graph!, which writes the validated
   graph into a Hyper cursor.  Client-side editor code lives in
   itera.graph.editor; the page lives in itera.page.graph."
  (:require [clojure.string :as str]))

(def empty-graph
  {:nodes [] :edges []})

;; --- normalization helpers --------------------------------------------------

(defn- ->keyword [x]
  (cond
    (keyword? x) x
    (string? x)  (keyword x)
    :else        nil))

(defn- ->id [x]
  (when (some? x) (str x)))

(defn- ->number
  "Coerce x to a number, falling back to default when x is missing or invalid."
  [x default]
  (cond
    (number? x) x
    (string? x) (let [s (str/trim x)]
                  (if (seq s)
                    (or (parse-long s) (parse-double s) default)
                    default))
    :else default))

(defn- map-get
  "Look up k in m, tolerating a string-keyed map for the same name."
  [m k]
  (cond
    (not (map? m)) nil
    (contains? m k) (get m k)
    (instance? clojure.lang.Named k) (get m (name k))
    :else nil))

;; --- node-type catalog ------------------------------------------------------

(defn- normalize-port [port]
  (let [id (or (->id (:id port)) "")]
    (assoc port
           :id id
           :label (or (:label port) id)
           :data-type (or (->keyword (:data-type port)) :any))))

(defn- normalize-field [field]
  (let [key  (->keyword (:key field))
        type (or (->keyword (:type field)) :text)]
    (cond-> (assoc field
                   :key key
                   :type type
                   :label (or (:label field) (some-> key name)))
      (= :select type)
      (update :options #(vec (or % []))))))

(defn- normalize-node-type [type]
  (let [k (->keyword (:type type))]
    (assoc type
           :type k
           :label (or (:label type) (some-> k name))
           :inputs (mapv normalize-port (:inputs type))
           :outputs (mapv normalize-port (:outputs type))
           :fields (mapv normalize-field (:fields type)))))

(defn normalize-node-types
  "Normalize a node-type catalog.

   Keywords :type and port :data-type, defaults :fields to [], ensures every
   port has a string :id, defaults port data types to :any, and normalizes
   field keys/types.  Returns {:types [...] :by-type {<type> <spec>}}."
  [types]
  (let [types (->> (or types [])
                   (keep (fn [type]
                           (let [k (->keyword (:type type))]
                             (when k (normalize-node-type type)))))
                   vec)]
    {:types   types
     :by-type (reduce (fn [m type] (assoc m (:type type) type)) {} types)}))

(defn node-catalog
  "Return the normalized catalog for either a raw node-type vector or an
   already-normalized {:types ... :by-type ...} map."
  [types]
  (if (and (map? types) (contains? types :by-type))
    types
    (normalize-node-types types)))

;; --- graph normalization ----------------------------------------------------

(defn- normalize-field-value [field value]
  (case (:type field)
    :number (if (nil? value)
              (:default field)
              (->number value (:default field)))
    :select (let [v (str (if (nil? value) (or (:default field) "") value))
                  options (mapv str (:options field))]
              (cond
                (empty? options) v
                (some #{v} options) v
                (some #{(str (:default field))} options) (str (:default field))
                :else (first options)))
    :text (str (if (nil? value) (or (:default field) "") value))
    (if (nil? value) (:default field) value)))

(defn- normalize-node-fields [type-spec node]
  (let [values (:fields node)]
    (reduce
     (fn [acc field]
       (let [k (:key field)]
         (if (nil? k)
           acc
           (let [v (map-get values k)]
             (if (or (contains? values k)
                     (contains? values (name k))
                     (contains? field :default))
               (assoc acc k (normalize-field-value field v))
               acc)))))
     {}
     (:fields type-spec))))

(defn- normalize-node [by-type node]
  (let [type (->keyword (:type node))
        spec (get by-type type)
        pos  (:position node)]
    (assoc node
           :id (or (->id (:id node)) "")
           :type type
           :position {:x (->number (map-get pos :x) 0)
                      :y (->number (map-get pos :y) 0)}
           :fields (if spec (normalize-node-fields spec node) {}))))

(defn- normalize-edge [edge]
  (assoc edge
         :id (or (->id (:id edge)) "")
         :source-node (->id (:source-node edge))
         :target-node (->id (:target-node edge))
         :source-port (->id (:source-port edge))
         :target-port (->id (:target-port edge))))

(defn normalize-graph
  "Normalize a graph for a node-type catalog: node/edge/port IDs become
   strings, node :type becomes a keyword, :fields is limited to fields
   declared by the node type, declared defaults are merged in, and
   :number fields are coerced to numbers."
  [types graph]
  (let [{:keys [by-type]} (node-catalog types)
        graph (if (map? graph) graph {})]
    {:nodes (mapv #(normalize-node by-type %) (:nodes graph))
     :edges (mapv normalize-edge (:edges graph))}))

;; --- validation -------------------------------------------------------------

(defn- errors-for [errors code data message]
  (conj errors (merge {:code code :message message} data)))

(defn- duplicate-node-ids [nodes]
  (->> nodes
       (map :id)
       frequencies
       (keep (fn [[id n]] (when (> n 1) id)))
       vec))

(defn- port-by-id [ports id]
  (some #(when (= id (:id %)) %) ports))

(defn- compatible-types? [a b]
  (or (= a b) (= :any a) (= :any b)))

(defn- edge-errors [errors by-type nodes-by-id edge]
  (let [source-id (:source-node edge)
        target-id (:target-node edge)
        source-node (get nodes-by-id source-id)
        target-node (get nodes-by-id target-id)
        source-spec (get by-type (:type source-node))
        target-spec (get by-type (:type target-node))
        source-port (when source-spec (port-by-id (:outputs source-spec) (:source-port edge)))
        target-port (when target-spec (port-by-id (:inputs target-spec) (:target-port edge)))
        errors (cond-> errors
                 (nil? source-node)
                 (errors-for :unknown-node {:edge (:id edge) :node source-id}
                             (str "Edge " (:id edge) " references unknown source node " source-id))

                 (nil? target-node)
                 (errors-for :unknown-node {:edge (:id edge) :node target-id}
                             (str "Edge " (:id edge) " references unknown target node " target-id))

                 (and source-node (nil? source-port))
                 (errors-for :unknown-port {:edge (:id edge) :node source-id :port (:source-port edge)}
                             (str "Edge " (:id edge) " references undeclared output port "
                                  (:source-port edge) " on node " source-id))

                 (and target-node (nil? target-port))
                 (errors-for :unknown-port {:edge (:id edge) :node target-id :port (:target-port edge)}
                             (str "Edge " (:id edge) " references undeclared input port "
                                  (:target-port edge) " on node " target-id)))]
    (if (and source-port target-port
             (not (compatible-types? (:data-type source-port) (:data-type target-port))))
      (errors-for errors :incompatible-ports
                  {:edge (:id edge)}
                  (str "Edge " (:id edge) " connects " (name (:data-type source-port))
                       " to " (name (:data-type target-port))
                       "; port types must match or be :any"))
      errors)))

(defn- find-cycle
  "Return a node id that lies on a cycle, or nil when the graph is acyclic."
  [ids adjacency]
  (let [state (volatile! {})]
    (letfn [(visit [id]
              (case (get @state id)
                :done nil
                :active id
                (do
                  (vswap! state assoc id :active)
                  (or (some visit (get adjacency id []))
                      (do (vswap! state assoc id :done) nil)))))]
      (some visit ids))))

(defn- cycle-error [errors nodes edges]
  (let [ids (set (map :id nodes))
        adjacency (reduce
                   (fn [m edge]
                     (if (and (contains? ids (:source-node edge))
                              (contains? ids (:target-node edge)))
                       (update m (:source-node edge) (fnil conj []) (:target-node edge))
                       m))
                   {}
                   edges)
        cyclic (find-cycle ids adjacency)]
    (if cyclic
      (errors-for errors :cycle {:node cyclic}
                  (str "Graph contains a cycle involving node " cyclic))
      errors)))

(defn validate-graph
  "Validate a normalized graph for a node-type catalog.

   Checks unique node ids, known node types, edge endpoints and declared
   ports, at most one edge into an input port, compatible port data types,
   and that the graph is acyclic.

   Returns {:ok? true} or {:ok? false :errors [{:code ... :message ...}]}."
  [types graph]
  (let [{:keys [by-type]} (node-catalog types)
        nodes (:nodes graph)
        edges (:edges graph)
        nodes-by-id (reduce (fn [m node] (assoc m (:id node) node)) {} nodes)
        errors (reduce
                (fn [errors node]
                  (if (contains? by-type (:type node))
                    errors
                    (errors-for errors :unknown-node-type {:node (:id node)}
                                (str "Node " (:id node) " has unknown type " (:type node)))))
                []
                nodes)
        errors (reduce
                (fn [errors id]
                  (errors-for errors :duplicate-node-id {:node id}
                              (str "Duplicate node id: " id)))
                errors
                (duplicate-node-ids nodes))
        errors (reduce
                (fn [errors edge] (edge-errors errors by-type nodes-by-id edge))
                errors
                edges)
        errors (reduce
                (fn [errors [[node port] n]]
                  (if (> n 1)
                    (errors-for errors :multiple-input-edges {:node node :port port}
                                (str "Input port " port " on node " node
                                     " has " n " incoming edges; at most one is allowed"))
                    errors))
                errors
                (frequencies (map (juxt :target-node :target-port) edges)))
        errors (cycle-error errors nodes edges)]
    (if (seq errors)
      {:ok? false :errors errors}
      {:ok? true})))

;; --- persistence ------------------------------------------------------------

(defn save-graph!
  "Normalize graph for the node-type catalog, validate it, and on success
   reset cursor to the normalized graph.

   Returns {:ok? true :graph <normalized>} or {:ok? false :errors [...]}.
   The cursor is left untouched on failure so callers can surface errors
   without losing the last saved graph."
  [types cursor graph]
  (let [catalog (node-catalog types)
        normalized (normalize-graph catalog graph)
        result (validate-graph catalog normalized)]
    (if (:ok? result)
      (do
        (reset! cursor normalized)
        {:ok? true :graph normalized})
      result)))

(defn current-graph
  "Return the graph stored in cursor.  This is the handoff point for Clojure
   consumers that execute the graph; the editor itself never runs it."
  [cursor]
  (when cursor @cursor))