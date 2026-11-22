(ns itera.model.ui
  "Model state ownership for the model page.

   `start-model` loads a model file, builds the runner, and keeps the loaded
   model in `:model-a` (in addition to the runner and its watch bookkeeping).
   Every mutation of the model goes through a function here: the graph editor
   and the interact sidebar never touch the model edn directly.

   Node shape (in `:model-a`):
     {:id \"V1StGXR8_Z5jdHi6\"  ; immutable, server-generated nano-id
      :label \"a-node\"          ; editable display name (need not be unique)
      :type :value|:formula
      :value/editor :option-ui|:edn ; required for :value nodes
      :show? bool :position {:x 0 :y 0}
      :value <any>            ; :value nodes
      :code \"(fn [..] ..)\"    ; :formula nodes
      :option-ui [<rows>]     ; :option-ui value nodes
      :spec <any>}            ; optional, plot nodes (persisted itera-spec-plot spec)

   `:id` is the node's permanent identity (also the X6 cell id and the value
   edges reference); `:label` is only what the editor and the interact tab
   display."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.pprint :as pprint]
   [clojure.string :as string]
   [missionary.core :as m]
   [nano-id.core :as nano-id]
   [itera.input.option :as input-option]
   [itera.model.core :as rc]
   [itera.model.formula :as f]
   [itera.util.clj :as util]
   [itera.value.protocol :as spec-plot])
  (:import
   [java.io File]))


;; ---------------------------------------------------------------------------
;; loading / normalization
;; ---------------------------------------------------------------------------

(defn- model-path [ctx]
  (or (:itera/model-path ctx)
      (throw (ex-info "missing :itera/model-path in ctx" {:ctx ctx}))))

(defn- model-file [ctx model-name]
  (io/file (model-path ctx) (str model-name ".edn")))

(defn model-exists?
  "True when <model-path>/<name>.edn exists as a regular file."
  [ctx model-name]
  (let [file (model-file ctx model-name)]
    (and (.exists ^File file)
         (.isFile ^File file))))

(defn available-models [ctx]
  (try
    (->> (.listFiles (io/file (model-path ctx)))
         (filter #(and (.isFile ^File %)
                       (string/ends-with? (.getName ^File %) ".edn")))
         (map #(string/replace (.getName ^File %) #"\.edn$" ""))
         (sort)
         (into []))
    (catch Exception _ [])))

(defn load-model [ctx model-name]
  (edn/read-string (slurp (model-file ctx model-name))))

(defn- normalize-position [p]
  (when (and (map? p) (some? (:x p)) (some? (:y p)))
    {:x (:x p) :y (:y p)}))

(def ^:private value-editors
  "The editors a :value node may declare in :value/editor."
  #{:option-ui :edn})

(defn- normalize-node [node]
  (let [id (str (:id node))
        type (keyword (:type node))
        value-editor (some-> (:value/editor node) keyword)]
    (when (and (= :value type)
               (not (contains? value-editors value-editor)))
      (throw (ex-info (str "Invalid :value/editor for node \"" id "\""
                           (when-let [label (:label node)]
                             (str " (" label ")"))
                           ": expected one of " (pr-str (sort value-editors))
                           ", got " (pr-str (:value/editor node)))
                      {:node-id id
                       :value/editor (:value/editor node)})))
    (cond-> (assoc (dissoc node :uid)
                   :id id
                   :label (str (or (:label node) id))
                   :type type
                   :show? (boolean (:show? node)))
      (= :value type)
      (assoc :value/editor value-editor)
      (some? (:position node))
      (assoc :position (normalize-position (:position node))))))

(defn- normalize-model [m]
  (-> m
      (update :nodes #(mapv normalize-node (or % [])))
      (update :edges #(vec (or % [])))))

;; ---------------------------------------------------------------------------
;; runner watch bookkeeping (unchanged helpers)
;; ---------------------------------------------------------------------------

(defn show? [{:keys [:node-dict-a]}]
  (->> @node-dict-a
       (filter (fn [[_ {:keys [show?]
                        :or {show? false}}]]
                 show?))
       (into {})))

(defn watch-cell [{:keys [runner showing disposers index]} node-id]
  (let [node (rc/get-node runner node-id)]
    (when-let [out (:out node)]
      (let [a (atom nil)
            r (m/reduce (fn [_ v]
                          (reset! a v)
                          nil) nil out)
            dispose! (r #(pr-str "started " node-id %)
                        #(pr-str "stoped " node-id %))]
        (swap! index conj node-id)
        (swap! showing assoc node-id a)
        (swap! disposers assoc node-id dispose!)))))

(defn unwatch-cell [{:keys [showing disposers index]} node-id]
  (when (contains? @showing node-id)
    (let [d! (get @disposers node-id)]
      (when d! (d!))
      (swap! index disj node-id)
      (swap! showing dissoc node-id)
      (swap! disposers dissoc node-id))))

;; ---------------------------------------------------------------------------
;; lifecycle
;; ---------------------------------------------------------------------------

(defn start-model
  "Load a model and build its runner, returning the model handle, or `nil`
   when <model-path>/<name>.edn does not exist."
  [ctx model-name]
  (when (model-exists? ctx model-name)
    (let [m (normalize-model (load-model ctx model-name))
          r (rc/create-runner ctx m)
          m (if (nil? (:ns-data m))
              (assoc m :ns-data (:ns-data r))
              m)
          this {:ctx ctx
                :model-name model-name
                :graph-ns (:graph-ns r)
                :runner r
                :model-a (atom m)
                :ns-eval-a (atom (:ns-eval r))
                :graph-rev (atom 0)
                :showing (atom {})
                :disposers (atom {})
                :index (atom #{})}
          start-nodes (-> r show? keys)]
      (doall (map #(watch-cell this %) start-nodes))
      this)))

(defn index-a [{:keys [index]}]
  index)

(defn currently-showing [{:keys [showing]}]
  (keys @showing))

(defn stop-model [this]
  (let [nodes-running (currently-showing this)]
    (doall (map #(unwatch-cell this %) nodes-running))))

(defn get-atom [{:keys [showing]} node-id]
  (get @showing node-id))

(defn node-by-id
  "The model node with :id `id`, or nil."
  [this id]
  (some #(when (= id (:id %)) %) (:nodes @(:model-a this))))

(defn graph-rev [this]
  (when-let [a (:graph-rev this)] @a))

(defn- bump-rev! [this]
  (when-let [a (:graph-rev this)] (swap! a inc)))

;; ---------------------------------------------------------------------------
;; ns-data
;; ---------------------------------------------------------------------------

(defn ns-data
  "The model's :ns-data string."
  [this]
  (:ns-data @(:model-a this)))

(defn set-ns-data!
  "Set :ns-data on the model and evaluate it.  Returns the clj-eval-raw result,
   or {:ex e} when evaluation throws.  Does not recompile formula nodes."
  [this ns-data]
  (swap! (:model-a this) assoc :ns-data ns-data)
  (let [result (try (util/clj-eval-raw ns-data)
                    (catch Exception e {:ex e}))]
    (when-let [a (:ns-eval-a this)] (reset! a result))
    result))

;; ---------------------------------------------------------------------------
;; ports
;; ---------------------------------------------------------------------------

(defn- formula-args-for-code [this code]
  (try
    (let [{:keys [args flow?]} (-> (util/compile-formula (:graph-ns this) code)
                                   (f/create-formula))]
      (if flow? [] (vec (or args []))))
    (catch Exception _ [])))

(defn node-ports
  "Compute a node's ports.  :value nodes have no inputs and one \"out\"
   output; :formula nodes take their inputs from the compiled formula's :args
   (flow-valued formulas have no inputs).  Also one \"out\" output."
  [this {:keys [id type code]}]
  (let [rn (rc/get-node (:runner this) id)
        args (cond
               (not= :formula type) []
               (and rn (contains? rn :args)) (vec (or (:args rn) []))
               :else (formula-args-for-code this code))]
    {:inputs (mapv (fn [a] {:id a :label a}) args)
     :outputs [{:id "out" :label "out"}]}))

;; ---------------------------------------------------------------------------
;; graph <-> editor
;; ---------------------------------------------------------------------------

(declare reconcile!)

(defn render-graph
  "JSON-friendly graph for the editor.  Each node's :id is the X6 cell id and
   the node :label is the displayed/editable name; edges reference node ids.

   Nodes carry topology/display data only — :code, :value, :value/editor and
   :option-ui stay on the server and are read from :model-a by `node-by-id`
   when the edit popup opens."
  [this]
  (let [m @(:model-a this)]
    {:nodes (mapv (fn [n]
                    (let [p (node-ports this n)
                          pos (or (:position n) {:x 0 :y 0})]
                      {:id (:id n)
                       :label (:label n)
                       :type (name (:type n))
                       :show? (boolean (:show? n))
                       :position {:x (:x pos) :y (:y pos)}
                       :inputs (:inputs p)
                       :outputs (:outputs p)}))
                  (:nodes m))
     :edges (mapv (fn [e]
                    {:id (:id e)
                     :source-node (:source-node e)
                     :source-port (:source-port e)
                     :target-node (:target-node e)
                     :target-port (:target-port e)})
                  (:edges m))}))

(defn- port-id-set [this node]
  (set (map :id (:inputs (node-ports this node)))))

(defn apply-graph!
  "Replace nodes/edges with the editor's serialized graph, unless the client's
   echoed `client-rev` is stale (a graph-change emitted before a server-side
   rebuild would otherwise omit a just-created node).

   Nodes are matched by :id; unknown ids are dropped (the server is the only
   creator).  Edges with unknown endpoints or undeclared target ports are
   dropped, and at most one edge may target an input port.

   Existing nodes keep their :id and :code/:value/:option-ui/:value/editor —
   those are mutated only through set-node-code / set-node-value /
   set-node-option-ui.  :label, :show? and :position come from the editor; a
   blank :label falls back to the previous label."
  [this graph client-rev]
  (when (= (graph-rev this) client-rev)
    (let [old @(:model-a this)
          old-by-id (into {} (map (juxt :id identity)) (:nodes old))
          new-nodes
          (into []
                (keep (fn [n]
                        (let [id (str (:id n))
                              o (get old-by-id id)
                              label (str (or (:label n) ""))]
                          (when o
                            (assoc o
                                   :label (if (string/blank? label)
                                            (:label o)
                                            label)
                                   :show? (boolean (:show? n))
                                   :position (or (normalize-position (:position n))
                                                 (:position o)))))))
                (:nodes graph))
          node-by-id (into {} (map (juxt :id identity)) new-nodes)
          seen-in (volatile! #{})
          new-edges
          (into []
                (keep (fn [e]
                        (let [sid (str (:source-node e))
                              tid (str (:target-node e))
                              tp (str (:target-port e))
                              tnode (get node-by-id tid)]
                          (when (and sid tid tnode
                                     (= "out" (str (:source-port e)))
                                     (contains? (port-id-set this tnode) tp)
                                     (not (contains? @seen-in [tid tp])))
                            (vswap! seen-in conj [tid tp])
                            {:id (str (or (:id e) (str "e-" (Math/abs (hash [sid tid tp])))))
                             :source-node sid
                             :source-port "out"
                             :target-node tid
                             :target-port tp}))))
                (:edges graph))
          new-model (assoc old :nodes new-nodes :edges new-edges)]
      (reset! (:model-a this) new-model)
      (reconcile! this old new-model))))

;; ---------------------------------------------------------------------------
;; reconcile (targeted where possible, else rebuild)
;; ---------------------------------------------------------------------------

(defn- edge-signature [model]
  (->> (:edges model)
       (map (fn [e] [(:source-node e) (:source-port e)
                     (:target-node e) (:target-port e)]))
       (sort-by pr-str)
       vec))

(defn rebuild!
  "Dispose watches and recreate the runner from :model-a, then re-watch the
   :show? nodes.  The runner map (and its atoms) is reused so callers keeping a
   reference to `this` see the fresh nodes."
  [this]
  (stop-model this)
  (let [r (:runner this)
        model @(:model-a this)
        fresh (rc/create-runner (:ctx this) model)]
    (reset! (:node-dict-a r) @(:node-dict-a fresh))
    (reset! (:edges-a r) @(:edges-a fresh))
    (doseq [id (-> r show? keys)]
      (watch-cell this id))
    this))

(defn- targeted-update! [this old new]
  (let [old-by-id (into {} (map (juxt :id identity)) (:nodes old))]
    (doseq [n (:nodes new)
            :let [o (get old-by-id (:id n))]
            :when o]
      (let [was? (boolean (:show? o))
            now? (boolean (:show? n))]
        (cond
          (and now? (not was?)) (watch-cell this (:id n))
          (and was? (not now?)) (unwatch-cell this (:id n)))))
    this))

(defn reconcile! [this old new]
  (let [old-ids (set (map :id (:nodes old)))
        new-ids (set (map :id (:nodes new)))
        old-by-id (into {} (map (juxt :id identity)) (:nodes old))
        code-changed? (some (fn [n]
                              (let [o (get old-by-id (:id n))]
                                (or (nil? o) (not= (:code o) (:code n)))))
                            (:nodes new))
        edges-changed? (not= (edge-signature old) (edge-signature new))]
    (if (or (not= old-ids new-ids) code-changed? edges-changed?)
      (rebuild! this)
      (targeted-update! this old new))))

;; ---------------------------------------------------------------------------
;; node creation (server-owned ids)
;; ---------------------------------------------------------------------------

(defn- fresh-node-id [this]
  (let [ids (set (map :id (:nodes @(:model-a this))))]
    (loop []
      (let [id (nano-id/nano-id 16)]
        (if (contains? ids id)
          (recur)
          id)))))

(defn- next-node-position [this]
  (let [i (count (:nodes @(:model-a this)))]
    {:x (+ 60 (* 260 (mod i 4)))
     :y (+ 60 (* 160 (quot i 4)))}))

(defn add-node!
  "Create a :value or :formula node server-side with a fresh nano-id, rebuild
   the runner, and bump `graph-rev` so the editor rebuilds from the server
   graph.  A :value node requires its editor as the third argument
   (`:option-ui` or `:edn`); formula nodes pass `nil`.  Returns `this`."
  [this type editor]
  (let [type (keyword (or type :value))]
    (when (contains? #{:value :formula} type)
      (let [editor (when (= :value type) (keyword editor))]
        (when (and (= :value type)
                   (not (contains? value-editors editor)))
          (throw (ex-info (str "A :value node needs :value/editor :option-ui "
                               "or :edn, got " (pr-str editor))
                          {:type type :value/editor editor})))
        (let [old @(:model-a this)
              node (cond-> {:id (fresh-node-id this)
                            :label (str (name type) "-" (inc (count (:nodes old))))
                            :type type
                            :show? false
                            :position (next-node-position this)}
                     (= :formula type) (assoc :code "(fn [ctx x] x)")
                     (= :value type) (assoc :value/editor editor
                                            :value (if (= :edn editor) nil {}))
                     (= :option-ui editor) (assoc :option-ui []))
              new (update old :nodes conj node)]
          (bump-rev! this)
          (reset! (:model-a this) new)
          (reconcile! this old new))))
    this))

;; ---------------------------------------------------------------------------
;; recompute a formula node + its downstream dependents
;; ---------------------------------------------------------------------------

(defn- formula-id-set [model]
  (->> (:nodes model)
       (filter #(= :formula (:type %)))
       (map :id)
       (set)))

(defn- descendant-formulas [model start-id]
  (let [adj (reduce (fn [m e]
                      (update m (:source-node e) (fnil conj []) (:target-node e)))
                    {}
                    (:edges model))
        fids (formula-id-set model)]
    (loop [seen #{} queue [start-id]]
      (if (empty? queue)
        (set (filter fids seen))
        (let [id (peek queue)]
          (if (contains? seen id)
            (recur seen (pop queue))
            (recur (conj seen id) (into (pop queue) (get adj id [])))))))))

(defn- topo-formula-order [model]
  (let [fids (formula-id-set model)
        fedges (filter #(and (contains? fids (:source-node %))
                             (contains? fids (:target-node %)))
                       (:edges model))
        deps (reduce (fn [m e]
                       (update m (:target-node e) (fnil conj #{}) (:source-node e)))
                     {}
                     fedges)]
    (loop [done #{} out [] remaining fids]
      (if (empty? remaining)
        out
        (let [ready (filter (fn [id] (every? done (get deps id #{}))) remaining)]
          (if (empty? ready)
            (into out remaining)
            (recur (into done ready)
                   (into out ready)
                   (reduce disj remaining ready))))))))

(defn- recompute-formula! [this start-id]
  (let [r (:runner this)
        model @(:model-a this)
        affected (descendant-formulas model start-id)]
    (doseq [id (filter affected (topo-formula-order model))]
      (let [shown? (contains? @(:showing this) id)]
        (when shown? (unwatch-cell this id))
        (rc/update-formula-node! r id)
        (when shown? (watch-cell this id))))
    this))

(defn- prune-edges [edges node-id args]
  (let [as (set args)]
    (vec (remove #(and (= node-id (:target-node %))
                       (not (contains? as (:target-port %))))
                 edges))))

(defn set-node-code
  "Update a formula node's :code, drop edges now pointing at removed args,
   recompile the node and its downstream formulas in place (never duplicating
   nodes), and ask the editor to re-read the graph (ports may have changed)."
  [this node-id code]
  (let [args (formula-args-for-code this code)
        r (:runner this)]
    (bump-rev! this)
    (swap! (:model-a this)
           (fn [m]
             (-> m
                 (update :edges prune-edges node-id args)
                 (update :nodes
                         (fn [ns]
                           (mapv (fn [n] (if (= node-id (:id n)) (assoc n :code code) n)) ns))))))
    (swap! (:edges-a r) prune-edges node-id args)
    (when (contains? @(:node-dict-a r) node-id)
      (swap! (:node-dict-a r) update node-id assoc :code code))
    (recompute-formula! this node-id)
    this))

(defn set-node-option-ui
  "Update a :value node's :option-ui rows, mark it :value/editor :option-ui,
   and re-initialize its :value."
  [this node-id rows]
  (let [rows (vec (or rows []))]
    (bump-rev! this)
    (swap! (:model-a this)
           (fn [m]
             (update m :nodes
                     (fn [ns]
                       (mapv (fn [n]
                               (if (= node-id (:id n))
                                 (assoc n :value/editor :option-ui
                                        :option-ui rows :value {})
                                 n))
                             ns)))))
    (when-let [node (rc/get-node (:runner this) node-id)]
      (when (:value-a node) (reset! (:value-a node) {})))
    this))

(defn set-node-value
  "Update a :value node's value (writes :model-a and its live :value-a)."
  [this node-id value]
  (swap! (:model-a this)
         (fn [m]
           (update m :nodes
                   (fn [ns]
                     (mapv (fn [n] (if (= node-id (:id n)) (assoc n :value value) n)) ns)))))
  (when-let [node (rc/get-node (:runner this) node-id)]
    (when (:value-a node) (reset! (:value-a node) value)))
  this)

(defn node-spec
  "Effective plot spec for node-id: the stored :spec if present, else the
   default-spec of the node's current value when it implements itera-spec-plot."
  [this node-id]
  (or (:spec (node-by-id this node-id))
      (when-let [a (get-atom this node-id)]
        (let [v @a]
          (when (satisfies? spec-plot/itera-spec-plot v)
            (spec-plot/default-spec v))))))

(defn set-node-spec
  "Persist a plot node's edited :spec into :model-a."
  [this node-id spec]
  (swap! (:model-a this)
         (fn [m]
           (update m :nodes
                   (fn [ns]
                     (mapv (fn [n] (if (= node-id (:id n)) (assoc n :spec spec) n)) ns)))))
  this)

;; ---------------------------------------------------------------------------
;; sidebar option-uis
;; ---------------------------------------------------------------------------

(defn option-ui-nodes
  "Every :value node whose :value/editor is :option-ui and whose :option-ui
   rows compile to a non-empty spec, as
   {:node-id <id> :label <label> :spec <compiled spec> :value <value>}."
  [this]
  (let [m @(:model-a this)]
    (->> (:nodes m)
         (filter #(and (= :value (:type %))
                       (= :option-ui (:value/editor %))))
         (keep (fn [n]
                 (let [spec (input-option/create-option-spec (:graph-ns this) (:option-ui n))]
                   (when (seq spec)
                     {:node-id (:id n)
                      :label (:label n)
                      :spec spec
                      :value (:value n)}))))
         vec)))

;; ---------------------------------------------------------------------------
;; positions / persistence
;; ---------------------------------------------------------------------------

(defn assign-missing-positions
  "Grid-place nodes whose :position is missing into the visible area."
  [this]
  (swap! (:model-a this)
         (fn [m]
           (update m :nodes
                   (fn [ns]
                     (vec (map-indexed
                           (fn [i node]
                             (if (:position node)
                               node
                               (assoc node :position
                                      {:x (+ 60 (* 260 (mod i 4)))
                                       :y (+ 60 (* 160 (quot i 4)))})))
                           ns))))))
  this)

(defn- persist-node [{:keys [id label type show? position value code option-ui spec]
                      value-editor :value/editor}]
  (cond-> (array-map :id id :label label :type type :show? (boolean show?))
    position (assoc :position {:x (:x position) :y (:y position)})
    (= :value type) (assoc :value/editor value-editor :value value)
    (= :formula type) (assoc :code code)
    (seq option-ui) (assoc :option-ui option-ui)
    (some? spec) (assoc :spec spec)))

(defn create-model!
  "Create an empty model file `<model-path>/<name>.edn` (plus its parent
   directories).  Never overwrites: an existing file is an error.

   The name is trimmed and must match `[A-Za-z0-9_-]+`.  Returns
   `{:ok? true :name <name>}` or `{:ok? false :error <message>}`."
  [ctx model-name]
  (let [name (string/trim (or model-name ""))]
    (cond
      (not (re-matches #"[A-Za-z0-9_-]+" name))
      {:ok? false
       :error "Model name must be non-blank and use only letters, digits, \"_\" and \"-\"."}

      (model-exists? ctx name)
      {:ok? false
       :error (str "Model \"" name "\" already exists.")}

      :else
      (try
        (let [file (model-file ctx name)]
          (io/make-parents file)
          (spit file (with-out-str
                       (binding [pprint/*print-right-margin* 110]
                         (pprint/pprint {:graph/name name
                                         :nodes []
                                         :edges []}))))
          {:ok? true :name name})
        (catch Exception e
          {:ok? false :error (str "Could not create model: " (ex-message e))})))))

(defn save-model-edn!
  "Serialize :model-a and write <path>/<name>.edn."
  [this]
  (let [model @(:model-a this)
        persisted (cond-> (array-map :graph/name (:graph/name model))
                    (some? (:ns-data model)) (assoc :ns-data (:ns-data model))
                    true (assoc :nodes (mapv persist-node (:nodes model))
                                :edges (mapv #(select-keys % [:id :source-node :source-port
                                                              :target-node :target-port])
                                             (:edges model))))
        file (model-file (:ctx this) (:model-name this))]
    (io/make-parents file)
    (spit file (with-out-str (binding [pprint/*print-right-margin* 110]
                               (pprint/pprint persisted))))))

(comment
  (def ctx {:itera/model-path "../models"})
  (available-models ctx)
  (load-model ctx "sum")
  (def x (start-model ctx "sum_ui"))
  (currently-showing x)
  (get-atom x (first (currently-showing x)))
  (render-graph x)
  (option-ui-nodes x)
  (assign-missing-positions x)
  (stop-model x))
