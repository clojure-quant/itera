(ns itera.graph.editor
  "AntV X6 node-graph editor as a Hyper client component.

   Attrs:
     graph        - canonical graph map {:nodes [...] :edges [...]}
     node-types   - normalized node-type catalog
     save-result  - {:status \"ok\"|\"error\"} from the last save action
     data-on:save - server action; receives a CustomEvent detail {:graph ...}

   The X6 instance is created once per mount (seamless `update` model).  The
   component edits the graph locally, tracks dirty state, and emits the
   canonical graph on Save; the page validates and persists it.

   Port rules are enforced while connecting: typed ports must match (or be
   :any), an input port accepts at most one edge, and the graph stays acyclic."
  (:require [hyper.core :as h]))

(h/defc itera-graph-editor
  "X6 graph editor web component."
  {:require [["https://esm.sh/@antv/x6@3.1.8?bundle" :as x6]]}
  [{:keys [graph node-types save-result]}]
  (render
   [:div {:style "display:flex;flex-direction:column;height:100%;min-height:0;font-family:system-ui,-apple-system,'Segoe UI',sans-serif"}
    [:div {:class "itera-graph-toolbar"
           :style "display:flex;flex-wrap:wrap;align-items:center;gap:0.35rem;padding:0.5rem;border-bottom:1px solid #e2e8f0;background:#fff;flex:0 0 auto"}
     [:span {:style "font-size:0.7rem;color:#64748b;margin-right:0.15rem"} "Add:"]
     (for [nt node-types]
       [:button {:type "button"
                 :title (str "Add " (aget nt "label") " node")
                 :on {:click (fn [_] ((.-add-node ctx) nt))}
                 :style "font-size:0.68rem;padding:0.18rem 0.42rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
        (str (aget nt "label")
             " (" (.-length (aget nt "inputs")) " in / "
             (.-length (aget nt "outputs")) " out)")])
     [:span {:class "itera-graph-status"
             :style "margin-left:auto;font-size:0.72rem;color:#475569;white-space:nowrap"}
      "Saved"]
     [:button {:type "button"
               :title "Delete the selected node or edge"
               :on {:click (fn [_] ((.-delete-selected ctx)))}
               :style "font-size:0.68rem;padding:0.18rem 0.42rem;border:1px solid #fca5a5;background:#fef2f2;border-radius:4px;cursor:pointer;color:#b91c1c"}
      "Delete"]
     [:button {:type "button"
               :title "Center the graph"
               :on {:click (fn [_] ((.-fit ctx)))}
               :style "font-size:0.68rem;padding:0.18rem 0.42rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
      "Fit"]
     [:button {:type "button"
               :title "Validate and save the graph"
               :on {:click (fn [_] ((.-save ctx)))}
               :style "font-size:0.68rem;padding:0.18rem 0.42rem;border:1px solid #86efac;background:#f0fdf4;border-radius:4px;cursor:pointer;color:#166534"}
      "Save"]]
    [:div {:class "itera-graph-canvas"
           :style "flex:1;min-height:0;position:relative"}]
    [:style ".itera-graph-canvas .x6-graph{width:100%;height:100%}.itera-graph-canvas foreignObject>body{margin:0;padding:0;overflow:visible}"]])
  (mount [root]
    (let [Shape (.-HTML (.-Shape x6))
          host (.querySelector root ".itera-graph-canvas")
          status (.querySelector root ".itera-graph-status")
          by-type (js-obj)
          groups (js-obj
                  "in" (js-obj
                        "position" "left"
                        "attrs" (js-obj
                                 "circle" (js-obj "r" 5 "magnet" true
                                                   "stroke" "#64748b" "strokeWidth" 1.5
                                                   "fill" "#fff")
                                 "text" (js-obj "fontSize" 9 "fill" "#475569"))
                        "label" (js-obj "position" "left"))
                  "out" (js-obj
                         "position" "right"
                         "attrs" (js-obj
                                  "circle" (js-obj "r" 5 "magnet" true
                                                    "stroke" "#64748b" "strokeWidth" 1.5
                                                    "fill" "#fff")
                                  "text" (js-obj "fontSize" 9 "fill" "#475569"))
                         "label" (js-obj "position" "right")))
          _ (.forEach node-types (fn [t] (aset by-type (aget t "type") t)))
          as-array (fn [v] (if v v (array)))
          type-spec (fn [t] (aget by-type t))
          node-height (fn [spec]
                        (let [fields (.-length (as-array (aget spec "fields")))
                              ins (.-length (as-array (aget spec "inputs")))
                              outs (.-length (as-array (aget spec "outputs")))
                              ports (max ins outs)]
                          (max 80
                               (+ 44 (* 34 fields))
                               (+ 30 (* 22 ports)))))
          default-fields (fn [spec]
                           (let [fields (js-obj)]
                             (.forEach (as-array (aget spec "fields"))
                                       (fn [fs]
                                         (let [d (aget fs "default")
                                               t (aget fs "type")
                                               v (cond
                                                   (some? d) d
                                                   (= t "number") 0
                                                   (= t "select")
                                                   (let [opts (as-array (aget fs "options"))]
                                                     (if (> (.-length opts) 0) (aget opts 0) ""))
                                                   :else "")]
                                           (aset fields (aget fs "key") v))))
                             fields))
          node-data (fn [spec fields]
                      (js-obj "type" (aget spec "type")
                              "label" (aget spec "label")
                              "fields" (or fields (default-fields spec))
                              "fieldSpecs" (as-array (aget spec "fields"))
                              "inputs" (as-array (aget spec "inputs"))
                              "outputs" (as-array (aget spec "outputs"))))
          copy-data (fn [data fields]
                      (js-obj "type" (aget data "type")
                              "label" (aget data "label")
                              "fields" fields
                              "fieldSpecs" (aget data "fieldSpecs")
                              "inputs" (aget data "inputs")
                              "outputs" (aget data "outputs")))
          port-by-id (fn [ports id]
                       (let [n (.-length ports)]
                         (loop [i 0]
                           (when (< i n)
                             (let [p (aget ports i)]
                               (if (= (aget p "id") id) p (recur (inc i))))))))
          compatible? (fn [a b]
                        (or (= a b) (= a "any") (= b "any")))
          style-edge (fn [edge]
                       (.attr edge "line/stroke" "#94a3b8")
                       (.attr edge "line/strokeWidth" 1.5)
                       (.attr edge "line/targetMarker"
                              (js-obj "name" "block" "width" 8 "height" 8)))
          set-status (fn [text color]
                       (when status
                         (set! (.-textContent status) text)
                         (set! (.-color (.-style status)) color)))
          mark-dirty (fn []
                       (when (not (.-loading ctx))
                         (set! (.-dirty ctx) true)
                         (set-status "Unsaved changes" "#b45309")))
          clear-highlight (fn [cell]
                            (if (.isNode cell)
                              (do (.attr cell "body/stroke" "none")
                                  (.attr cell "body/strokeWidth" 0))
                              (do (.attr cell "line/stroke" "#94a3b8")
                                  (.attr cell "line/strokeWidth" 1.5))))
          highlight (fn [cell]
                      (if (.isNode cell)
                        (do (.attr cell "body/stroke" "#2563eb")
                            (.attr cell "body/strokeWidth" 2))
                        (do (.attr cell "line/stroke" "#2563eb")
                            (.attr cell "line/strokeWidth" 2.5))))
          select-cell (fn [cell]
                        (when-let [prev (.-selected ctx)]
                          (clear-highlight prev))
                        (set! (.-selected ctx) cell)
                        (when cell (highlight cell)))
          drop-dangling-edges
          (fn []
            (let [g (.-graph ctx)
                  bad (array)]
              (when g
                (.forEach (.getEdges g)
                          (fn [e]
                            (when (or (nil? (.getSourceCellId e))
                                      (nil? (.getTargetCellId e)))
                              (.push bad e))))
                (.forEach bad (fn [e] (.removeCell g e))))))
          input-taken? (fn [g cell port]
                         (let [edges (.getEdges g)
                               id (.-id cell)
                               n (.-length edges)]
                           (loop [i 0]
                             (when (< i n)
                               (let [e (aget edges i)]
                                 (if (and (= (.getTargetCellId e) id)
                                          (= (.getTargetPortId e) port))
                                   true
                                   (recur (inc i))))))))
          creates-cycle? (fn [g source-cell target-cell]
                           (let [source-id (.-id source-cell)
                                 edges (.getEdges g)
                                 stack (array (.-id target-cell))
                                 seen (js-obj)]
                             (loop []
                               (when (> (.-length stack) 0)
                                 (let [id (.pop stack)]
                                   (if (= id source-id)
                                     true
                                     (if (aget seen id)
                                       (recur)
                                       (do
                                         (aset seen id true)
                                         (.forEach edges
                                                  (fn [e]
                                                    (when (= (.getSourceCellId e) id)
                                                      (.push stack (.getTargetCellId e)))))
                                         (recur)))))))))
          validate-connection
          (fn [args]
            (let [g (.-graph ctx)
                  source-cell (aget args "sourceCell")
                  target-cell (aget args "targetCell")
                  source-port (aget args "sourcePort")
                  target-port (aget args "targetPort")]
              (and g source-cell target-cell source-port target-port
                   (let [sd (.getData source-cell)
                         td (.getData target-cell)
                         sp (port-by-id (as-array (aget sd "outputs")) source-port)
                         tp (port-by-id (as-array (aget td "inputs")) target-port)]
                     (and sp tp
                          (compatible? (aget sp "data-type") (aget tp "data-type"))
                          (not (input-taken? g target-cell target-port))
                          (not (creates-cycle? g source-cell target-cell)))))))
          graph-instance
          (new x6/Graph
               (js-obj
                "container" host
                "autoResize" true
                "background" (js-obj "color" "#f8fafc")
                "grid" (js-obj "visible" true "type" "dot" "size" 12)
                "panning" (js-obj "enabled" true)
                "mousewheel" (js-obj "enabled" true
                                     "modifiers" (array "ctrl" "meta")
                                     "minScale" 0.2 "maxScale" 4)
                "connecting" (js-obj
                              "allowBlank" false
                              "allowLoop" false
                              "allowNode" false
                              "allowEdge" false
                              "allowPort" true
                              "allowMulti" false
                              "snap" true
                              "router" (js-obj "name" "orth")
                              "connector" (js-obj "name" "rounded"
                                                  "args" (js-obj "radius" 8))
                              "validateConnection" validate-connection)))
          render-html
          (fn [cell]
            (let [data (.getData cell)
                  fields (or (aget data "fields") (js-obj))
                  specs (as-array (aget data "fieldSpecs"))
                  ins (as-array (aget data "inputs"))
                  outs (as-array (aget data "outputs"))
                  outer (.createElement js/document "div")
                  card (.createElement js/document "div")
                  header (.createElement js/document "div")
                  title (.createElement js/document "div")
                  badge (.createElement js/document "span")]
              (set! (.-cssText (.-style outer))
                    "width:100%;height:100%;box-sizing:border-box;padding:6px 13px")
              (set! (.-cssText (.-style card))
                    "width:100%;height:100%;box-sizing:border-box;border:1px solid #94a3b8;border-radius:6px;background:#fff;padding:5px 7px;font-size:11px;color:#0f172a;overflow:hidden")
              (set! (.-cssText (.-style header))
                    "display:flex;justify-content:space-between;align-items:center;gap:4px;font-weight:600;font-size:12px;line-height:16px")
              (set! (.-textContent title) (or (aget data "label") (aget data "type") ""))
              (set! (.-cssText (.-style badge))
                    "font-weight:400;font-size:9px;color:#64748b;white-space:nowrap")
              (set! (.-textContent badge)
                    (str (.-length ins) " in / " (.-length outs) " out"))
              (.appendChild header title)
              (.appendChild header badge)
              (.appendChild card header)
              (.forEach specs
                        (fn [fs]
                          (let [key (aget fs "key")
                                ftype (aget fs "type")
                                row (.createElement js/document "div")
                                label (.createElement js/document "span")
                                input (if (= ftype "select")
                                        (.createElement js/document "select")
                                        (.createElement js/document "input"))
                                value (aget fields key)]
                            (set! (.-cssText (.-style row))
                                  "display:flex;align-items:center;gap:5px;margin-top:5px;font-size:10px;line-height:16px")
                            (set! (.-textContent label) (or (aget fs "label") key))
                            (set! (.-cssText (.-style label))
                                  "width:58px;flex:0 0 auto;color:#374151")
                            (set! (.-cssText (.-style input))
                                  "flex:1;min-width:0;font-size:10px;padding:1px 3px;border:1px solid #cbd5e1;border-radius:3px;box-sizing:border-box")
                            (if (= ftype "select")
                              (let [opts (as-array (aget fs "options"))]
                                (.forEach opts
                                          (fn [o]
                                            (let [option (.createElement js/document "option")]
                                              (set! (.-value option) (str o))
                                              (set! (.-textContent option) (str o))
                                              (when (= (str value) (str o))
                                                (set! (.-selected option) true))
                                              (.appendChild input option))))
                                (when (and (nil? value) (> (.-length opts) 0))
                                  (set! (.-value input) (str (aget opts 0)))))
                              (do
                                (set! (.-type input) (if (= ftype "number") "number" "text"))
                                (set! (.-value input)
                                      (str (if (nil? value)
                                             (let [d (aget fs "default")]
                                               (if (nil? d) "" d))
                                             value)))))
                            (.addEventListener
                             input "change"
                             (fn [_]
                               (let [ty (aget fs "type")
                                     raw (.-value input)
                                     v (if (= ty "number")
                                         (let [n (js/parseFloat raw)]
                                           (if (js/isNaN n) (aget fs "default") n))
                                         raw)
                                     next-fields (js-obj)]
                                 (.forEach (js/Object.keys fields)
                                           (fn [k] (aset next-fields k (aget fields k))))
                                 (aset next-fields key v)
                                 (.setData cell (copy-data data next-fields)))))
                            (.appendChild row label)
                            (.appendChild row input)
                            (.appendChild card row))))
              (.appendChild outer card)
              outer))
          _ (when (and Shape host)
              (.register Shape
                         (js-obj "shape" "itera-graph-node"
                                 "width" 240
                                 "height" 100
                                 "effect" (array "data")
                                 "html" render-html)))
          build-ports
          (fn [spec]
            (let [items (array)]
              (.forEach (as-array (aget spec "inputs"))
                        (fn [p]
                          (.push items
                                 (js-obj "id" (aget p "id")
                                         "group" "in"
                                         "attrs" (js-obj "text" (js-obj "text" (aget p "label")))))))
              (.forEach (as-array (aget spec "outputs"))
                        (fn [p]
                          (.push items
                                 (js-obj "id" (aget p "id")
                                         "group" "out"
                                         "attrs" (js-obj "text" (js-obj "text" (aget p "label")))))))
              (js-obj "groups" groups "items" items)))
          edge-attrs
          (fn []
            (js-obj "line"
                    (js-obj "stroke" "#94a3b8"
                            "strokeWidth" 1.5
                            "targetMarker" (js-obj "name" "block" "width" 8 "height" 8))))
          build-graph!
          (fn [g]
            (let [data (or g (js-obj "nodes" (array) "edges" (array)))
                  nodes (array)
                  edges (array)
                  node-ids (js-obj)]
              (.forEach (as-array (aget data "nodes"))
                        (fn [n]
                          (let [spec (type-spec (aget n "type"))
                                pos (aget n "position")]
                            (when spec
                              (aset node-ids (aget n "id") true)
                              (.push nodes
                                     (js-obj "id" (aget n "id")
                                             "shape" "itera-graph-node"
                                             "x" (aget pos "x")
                                             "y" (aget pos "y")
                                             "width" 240
                                             "height" (node-height spec)
                                             "data" (node-data spec (or (aget n "fields") (js-obj)))
                                             "ports" (build-ports spec)))))))
              (.forEach (as-array (aget data "edges"))
                        (fn [e]
                          (let [src (aget e "source-node")
                                tgt (aget e "target-node")]
                            (when (and (aget node-ids src) (aget node-ids tgt))
                              (.push edges
                                     (js-obj "id" (aget e "id")
                                             "source" (js-obj "cell" src
                                                              "port" (aget e "source-port"))
                                             "target" (js-obj "cell" tgt
                                                              "port" (aget e "target-port"))
                                             "attrs" (edge-attrs)))))))
              (.fromJSON graph-instance (js-obj "nodes" nodes "edges" edges))))
          serialize
          (fn []
            (let [nodes (array)
                  edges (array)]
              (.forEach (.getNodes graph-instance)
                        (fn [n]
                          (let [d (.getData n)
                                pos (.getPosition n)]
                            (.push nodes
                                   (js-obj "id" (.-id n)
                                           "type" (aget d "type")
                                           "position" (js-obj "x" (.-x pos)
                                                              "y" (.-y pos))
                                           "fields" (or (aget d "fields") (js-obj)))))))
              (.forEach (.getEdges graph-instance)
                        (fn [e]
                          (when (and (.getSourceCellId e) (.getTargetCellId e))
                            (.push edges
                                   (js-obj "id" (.-id e)
                                           "source-node" (.getSourceCellId e)
                                           "source-port" (.getSourcePortId e)
                                           "target-node" (.getTargetCellId e)
                                           "target-port" (.getTargetPortId e))))))
              (js-obj "nodes" nodes "edges" edges)))
          add-node
(fn [spec]
            (let [n (.-length (.getNodes graph-instance))
                  node (.addNode graph-instance
                                 (js-obj
                                  "shape" "itera-graph-node"
                                  "x" (+ 40 (* 260 (mod n 4)))
                                  "y" (+ 40 (* 140 (mod n 3)))
                                  "width" 240
                                  "height" (node-height spec)
                                  "data" (node-data spec nil)
                                  "ports" (build-ports spec)))]
              (mark-dirty)
              (select-cell node)
              node))
          delete-selected
          (fn []
            (when-let [cell (.-selected ctx)]
              (.removeCell graph-instance cell)
              (set! (.-selected ctx) nil)
              (mark-dirty)))
          fit (fn [] (.centerContent graph-instance))
          save
          (fn []
            (drop-dangling-edges)
            (set-status "Saving..." "#475569")
            (emit "save" (js-obj "graph" (serialize))))
          key-handler
          (fn [evt]
            (let [target (.-target evt)
                  tag (.-tagName target)
                  typing (or (= tag "INPUT") (= tag "SELECT") (= tag "TEXTAREA")
                             (.-isContentEditable target))]
              (when (and (not typing) (= (.-key evt) "Delete"))
                (.preventDefault evt)
                (delete-selected))))
          rebuild
          (fn [g]
            (set! (.-loading ctx) true)
            (set! (.-selected ctx) nil)
            (build-graph! g)
            (set! (.-loading ctx) false)
            (set! (.-dirty ctx) false)
            (set! (.-lastGraph ctx) (js/JSON.stringify g))
            (set-status "Saved" "#475569")
            (.centerContent graph-instance))]
      (set! (.-graph ctx) graph-instance)
      (set! (.-selected ctx) nil)
      (set! (.-dirty ctx) false)
      (set! (.-loading ctx) true)
      (set! (.-add-node ctx) add-node)
      (set! (.-delete-selected ctx) delete-selected)
      (set! (.-fit ctx) fit)
      (set! (.-save ctx) save)
      (set! (.-rebuild ctx) rebuild)
      (set! (.-statusEl ctx) status)
      (set! (.-setStatus ctx) set-status)
      (set! (.-saveStatus ctx) nil)
      (.addEventListener host "click"
                         (fn [evt]
                           (let [target (.-target evt)
                                 tag (.-tagName target)
                                 typing (or (= tag "INPUT") (= tag "SELECT") (= tag "TEXTAREA")
                                            (.-isContentEditable target))]
                             (when-not typing
                               (let [view (.findViewByElem graph-instance target)]
                                 (select-cell (when view (.-cell view))))))))
      (.on graph-instance "node:added" (fn [_] (mark-dirty)))
      (.on graph-instance "node:removed" (fn [_] (mark-dirty)))
      (.on graph-instance "edge:added" (fn [args]
                                         (style-edge (aget args "edge"))
                                         (mark-dirty)))
      (.on graph-instance "edge:removed" (fn [_] (mark-dirty)))
      (.on graph-instance "node:change:position" (fn [_] (mark-dirty)))
      (.on graph-instance "node:change:data" (fn [_] (mark-dirty)))
      (.on graph-instance "edge:change:source" (fn [_] (mark-dirty)))
      (.on graph-instance "edge:change:target" (fn [_] (mark-dirty)))
      (build-graph! graph)
      (set! (.-loading ctx) false)
      (set! (.-lastGraph ctx) (js/JSON.stringify graph))
      (set-status "Saved" "#475569")
      (.centerContent graph-instance)
      (.addEventListener js/document "keydown" key-handler)
      (set! (.-keyHandler ctx) key-handler)
      (set! (.-mouseupHandler ctx)
            (fn [_] (js/setTimeout drop-dangling-edges 0)))
      (.addEventListener js/document "mouseup" (.-mouseupHandler ctx))))
  (update [_root]
    (let [incoming (or graph (js-obj "nodes" (array) "edges" (array)))
          s (js/JSON.stringify incoming)
          save-status (when save-result (aget save-result "status"))]
      (when (and save-status (not= save-status (.-saveStatus ctx)))
        (set! (.-saveStatus ctx) save-status)
        (if (= save-status "ok")
          (do
            (set! (.-dirty ctx) false)
            ((.-setStatus ctx) "Saved" "#475569"))
          (do
            (set! (.-dirty ctx) true)
            ((.-setStatus ctx) "Unsaved changes" "#b45309"))))
      (when (not= s (.-lastGraph ctx))
        (when-let [rebuild (.-rebuild ctx)]
          (rebuild incoming)))))
  (unmount [_root]
    (when-let [g (.-graph ctx)]
      (.dispose g))
    (when-let [kh (.-keyHandler ctx)]
      (.removeEventListener js/document "keydown" kh))
    (when-let [mh (.-mouseupHandler ctx)]
      (.removeEventListener js/document "mouseup" mh))
    (set! (.-graph ctx) nil)
    (set! (.-keyHandler ctx) nil)
    (set! (.-mouseupHandler ctx) nil)))