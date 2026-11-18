(ns itera.model-ui.graph-editor
  "Model-graph editor in two layers: the server-side `graph-editor` wrapper and
   the client-side `itera-model-graph-editor` canvas component.

   `graph-editor` takes the model runner handle and owns every server action
   the component emits: it derives the rendered graph and its revision from
   itera.runner.ui, creates the popup tab cursors,
   and builds the graph-change / edit actions.  It also owns the toolbar's
   Add/Save actions.  The node-edit popup also lives here: CodeMirror for
   :formula nodes and itera.input.option for :value nodes, writing the node
   through itera.runner.ui when the popup closes.

   It renders the toolbar and the component as siblings; the toolbar is not a
   child of the custom element.  Add (:value / :formula) and Save are
   :data-on:click server actions: Add runs itera.runner.ui/add-node! and the
   server replies by bumping graph-rev so the component rebuilds; Save runs
   itera.runner.ui/save-model-edn!.  The remaining
   toolbar buttons carry :data-cmd (delete / zoom-in / zoom-out / fit), and
   the component, on mount, attaches one delegated click listener to its
   closest `.itera-graph-editor` wrapper; those commands run client-local with
   no request.

   The component renders only the scroll/canvas column: node types are just
   :value and :formula, ports come from the server (formula :args), node boxes
   expose an editable label, an edit box and a show/hide eye, and the graph
   area scrolls (outer overflow:auto over a content-sized X6 host) with zoom
   in/out.  Nodes carry topology/display data only (:id, :label, :type, :show?,
   :position, :inputs, :outputs) — :code, :value and :option-ui stay on the
   server, where the edit popup resolves them by node id through
   itera.runner.ui/node-by-id.

   Graph nodes carry :id (the X6 cell id, an immutable server-generated
   nano-id) and :label (displayed/editable, may repeat).  Creating a node is
   server-owned: the Add buttons run the add-node! action and the server
   replies with a rebuilt graph.

   Component attrs (supplied by `graph-editor`):
     graph        - server-rendered graph {:nodes [...] :edges [...]} where
                    each node's :id is the X6 cell id and :label is the display
                    name, plus ports
     graph-rev    - server counter; a change forces the client to rebuild from
                    `graph` (used when ports change after a code edit and after
                    a server-created node)
     data-on:graph-change - server action; detail {:graph <serialized>
                                                   :graph-rev <rev>}
     data-on:edit         - server action; detail {:node-id <id> :type <type>}

   The client edits the graph locally and emits the full serialized graph on
   every structural change (delete/connect/disconnect/drag/show?/label), so the
   server can reconcile without the client rebuilding on its own echo."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.input.option :as input-opt]
            [itera.runner.ui :as rui]))

(h/defc itera-model-graph-editor
  "X6 canvas web component.  The toolbar lives in the graph-editor wrapper."
  {:require [["https://esm.sh/@antv/x6@3.1.8?bundle" :as x6]]}
  [{:keys [graph graph-rev]}]
  (render
   [:div {:style "display:flex;flex-direction:column;height:100%;min-height:0;font-family:system-ui,-apple-system,'Segoe UI',sans-serif"}
    [:div {:class "itera-model-scroll"
           :style "flex:1;min-height:0;overflow:auto;position:relative;background:#f8fafc"}
     [:div {:class "itera-model-canvas"
            :style "position:relative;width:100%;height:100%;overflow:hidden"}]]
    [:style ".itera-model-canvas .x6-graph{width:100%;height:100%}.itera-model-canvas foreignObject>body{margin:0;padding:0;overflow:visible}"]])
  (mount [root]
    (let [Shape (.-HTML (.-Shape x6))
          wrap (.querySelector root ".itera-model-scroll")
          host (.querySelector root ".itera-model-canvas")
          host-el (.-host (.getRootNode root))
          container (.closest host-el ".itera-graph-editor")
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
          as-array (fn [v] (if v v (array)))
          editable? (fn [n]
                      (when n
                        (let [tag (.-tagName n)]
                          (or (= tag "INPUT") (= tag "SELECT") (= tag "TEXTAREA")
                              (.-isContentEditable n)))))
          editable-in-path? (fn [evt]
                              (let [path (if (.-composedPath evt)
                                           (.composedPath evt)
                                           (array (.-target evt)))
                                    n (.-length path)]
                                (loop [i 0]
                                  (if (< i n)
                                    (if (editable? (aget path i))
                                      true
                                      (recur (inc i)))
                                    false))))
          typing? (fn [evt]
                    (or (editable? (.-target evt))
                        (editable-in-path? evt)
                        (editable? js/document.activeElement)
                        (let [el js/document.activeElement
                              sr (when el (.-shadowRoot el))]
                          (editable? (when sr (.-activeElement sr))))))
          node-height (fn [ins outs]
                        (max 74 (+ 44 (* 22 (max (.-length (as-array ins))
                                                 (.-length (as-array outs)))))))
          port-by-id (fn [ports id]
                       (let [n (.-length ports)]
                         (loop [i 0]
                           (when (< i n)
                             (let [p (aget ports i)]
                               (if (= (aget p "id") id) p (recur (inc i))))))))
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
                          (not (input-taken? g target-cell target-port))
                          (not (creates-cycle? g source-cell target-cell)))))))
          graph-instance
          (new x6/Graph
               (js-obj
                "container" host
                "autoResize" false
                "background" (js-obj "color" "#f8fafc")
                "grid" (js-obj "visible" true "type" "dot" "size" 12)
                "panning" (js-obj "enabled" false)
                "mousewheel" (js-obj "enabled" false)
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
          emit-graph! (fn []
                        (when-not (.-loading ctx)
                          (let [ser ((.-serialize ctx))
                                sig (js/JSON.stringify ser)]
                            (when (not= sig (.-lastSig ctx))
                              (set! (.-lastSig ctx) sig)
                              (emit "graph-change"
                                    (js-obj "graph" ser
                                            "graph-rev" (.-rev ctx)))))))
          clone-data (fn [d overrides]
                       (let [o (js-obj)]
                         (.forEach (array "label" "type" "show?" "inputs"
                                          "outputs")
                                   (fn [k] (aset o k (aget d k))))
                         (.assign js/Object o overrides)
                         o))
          render-html
          (fn [cell]
            (let [data (.getData cell)
                  type-kw (aget data "type")
                  label (aget data "label")
                  shown? (aget data "show?")
                  outer (.createElement js/document "div")
                  card (.createElement js/document "div")
                  header (.createElement js/document "div")
                  type-el (.createElement js/document "span")
                  tools (.createElement js/document "div")
                  id-input (.createElement js/document "input")
                  edit-btn (.createElement js/document "button")
                  eye-btn (.createElement js/document "button")]
              (set! (.-cssText (.-style outer))
                    "width:100%;height:100%;box-sizing:border-box;padding:6px 13px")
              (set! (.-cssText (.-style card))
                    "width:100%;height:100%;box-sizing:border-box;border:1px solid #94a3b8;border-radius:6px;background:#fff;padding:5px 7px;font-size:11px;color:#0f172a;overflow:hidden;display:flex;flex-direction:column;gap:4px")
              (set! (.-cssText (.-style header))
                    "display:flex;justify-content:space-between;align-items:center;gap:4px")
              (set! (.-textContent type-el) (str ":" type-kw))
              (set! (.-cssText (.-style type-el))
                    "font-weight:600;font-size:11px;color:#334155")
              (set! (.-cssText (.-style tools)) "display:flex;gap:2px;align-items:center")
              (set! (.-type edit-btn) "button")
              (set! (.-textContent edit-btn) "✏️")
              (set! (.-title edit-btn) "Edit")
              (set! (.-cssText (.-style edit-btn))
                    "cursor:pointer;border:none;background:transparent;padding:0;font-size:12px;line-height:1")
              (set! (.-type eye-btn) "button")
              (set! (.-textContent eye-btn) "👁")
              (set! (.-title eye-btn) "Show / hide")
              (set! (.-cssText (.-style eye-btn))
                    "cursor:pointer;border:none;background:transparent;padding:0;font-size:12px;line-height:1")
              (set! (.-opacity (.-style eye-btn)) (if shown? "1" "0.3"))
              (set! (.-filter (.-style eye-btn)) (if shown? "none" "grayscale(1)"))
              (set! (.-type id-input) "text")
              (set! (.-value id-input) (or label ""))
              (set! (.-cssText (.-style id-input))
                    "width:100%;box-sizing:border-box;font-size:10px;padding:1px 3px;border:1px solid #cbd5e1;border-radius:3px")
              (.appendChild tools edit-btn)
              (.appendChild tools eye-btn)
              (.appendChild header type-el)
              (.appendChild header tools)
              (.appendChild card header)
              (.appendChild card id-input)
              (.appendChild outer card)
              (.addEventListener
               edit-btn "click"
               (fn [_]
                 (let [d (.getData cell)]
                   (emit "edit" (js-obj "node-id" (.-id cell)
                                        "type" (aget d "type"))))))
              (.addEventListener
               eye-btn "click"
               (fn [_]
                 (let [d (.getData cell)
                       next (not (aget d "show?"))]
                   (.setData cell (clone-data d (js-obj "show?" next)))
                   (emit-graph!))))
              (.addEventListener
               id-input "change"
               (fn [_]
                 (let [d (.getData cell)
                       raw (.trim (.-value id-input))
                       cur (aget d "label")
                       val (if (> (.-length raw) 0) raw cur)]
                   (if (= val cur)
                     (set! (.-value id-input) cur)
                     (do
                       (.setData cell (clone-data d (js-obj "label" val)))
                       (emit-graph!))))))
              outer))
          _ (when (and Shape host)
              (.register Shape
                         (js-obj "shape" "itera-model-node"
                                 "width" 240
                                 "height" 90
                                 "effect" (array "data")
                                 "html" render-html)))
          build-ports
          (fn [ins outs]
            (let [items (array)]
              (.forEach (as-array ins)
                        (fn [p]
                          (.push items
                                 (js-obj "id" (aget p "id")
                                         "group" "in"
                                         "attrs" (js-obj "text" (js-obj "text" (aget p "label")))))))
              (.forEach (as-array outs)
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
          refresh-size!
          (fn []
            (let [acc (js-obj "minx" js/Infinity "maxx" (- js/Infinity)
                              "miny" js/Infinity "maxy" (- js/Infinity))
                  upd (fn [x y w h]
                        (aset acc "minx" (js/Math.min (aget acc "minx") x))
                        (aset acc "miny" (js/Math.min (aget acc "miny") y))
                        (aset acc "maxx" (js/Math.max (aget acc "maxx") (+ x w)))
                        (aset acc "maxy" (js/Math.max (aget acc "maxy") (+ y h))))]
              (.forEach (.getNodes graph-instance)
                        (fn [n]
                          (let [p (.getPosition n)
                                sz (.getSize n)]
                            (upd (.-x p) (.-y p) (.-width sz) (.-height sz)))))
              (.forEach (.getEdges graph-instance)
                        (fn [e]
                          (let [b (.getBBox e)]
                            (when (and b (.-width b) (.-height b))
                              (upd (.-x b) (.-y b) (.-width b) (.-height b))))))
              (if (< (aget acc "maxx") (aget acc "minx"))
                (do
                  (set! (.-lastW ctx) nil)
                  (set! (.-lastH ctx) nil)
                  (set! (.-width (.-style host)) "100%")
                  (set! (.-height (.-style host)) "100%")
                  (.resize graph-instance (.-clientWidth wrap) (.-clientHeight wrap)))
                (let [pad 40
                      scale (or (.-scale ctx) 1)
                      ;; clientWidth/Height is the wrapper content box
                      ;; (excluding any current scrollbar).  The canvas is
                      ;; overflow:hidden, so a host equal to it never creates a
                      ;; scrollbar; a larger host does.  Settling is handled by
                      ;; the single re-run below, which stops once the size
                      ;; reaches a fixpoint.
                      avail-w (.-clientWidth wrap)
                      avail-h (.-clientHeight wrap)
                      need-w (+ (* (- (aget acc "maxx") (aget acc "minx")) scale) (* 2 pad))
                      need-h (+ (* (- (aget acc "maxy") (aget acc "miny")) scale) (* 2 pad))
                      w (js/Math.max need-w avail-w)
                      h (js/Math.max need-h avail-h)]
                  (when (or (not= w (.-lastW ctx))
                            (not= h (.-lastH ctx))
                            (not= scale (.-lastScale ctx)))
                    (set! (.-lastW ctx) w)
                    (set! (.-lastH ctx) h)
                    (set! (.-lastScale ctx) scale)
                    (set! (.-width (.-style host)) (str w "px"))
                    (set! (.-height (.-style host)) (str h "px"))
                    (.resize graph-instance w h)
                    ;; x6 maps graph coords to screen as (coord * scale + t);
                    ;; graph.translate sets that translation absolutely, so
                    ;; this puts the content's top-left at (pad, pad)
                    ;; regardless of where the content sits.
                    (.translate graph-instance
                                (- pad (* (aget acc "minx") scale))
                                (- pad (* (aget acc "miny") scale)))
                    (when-not (.-pendingR ctx)
                      (set! (.-pendingR ctx) true)
                      (js/setTimeout
                       (fn []
                         (set! (.-pendingR ctx) false)
                         ((.-resizeFn ctx)))
                       0)))))))
          build-graph!
          (fn [g]
            (let [data (or g (js-obj "nodes" (array) "edges" (array)))
                  nodes (array)
                  edges (array)
                  node-ids (js-obj)]
              (.forEach (as-array (aget data "nodes"))
                        (fn [n]
                          (let [node-id (aget n "id")
                                pos (or (aget n "position") (js-obj "x" 0 "y" 0))
                                ins (as-array (aget n "inputs"))
                                outs (as-array (aget n "outputs"))]
                            (aset node-ids node-id true)
                            (.push nodes
                                   (js-obj "id" node-id
                                           "shape" "itera-model-node"
                                           "x" (aget pos "x")
                                           "y" (aget pos "y")
                                           "width" 240
                                           "height" (node-height ins outs)
                                           "data" (js-obj "label" (aget n "label")
                                                          "type" (aget n "type")
                                                          "show?" (aget n "show?")
                                                          "inputs" ins
                                                          "outputs" outs)
                                           "ports" (build-ports ins outs))))))
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
              (.fromJSON graph-instance (js-obj "nodes" nodes "edges" edges))
              (set! (.-lastSig ctx) (js/JSON.stringify ((.-serialize ctx))))))
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
                                           "label" (aget d "label")
                                           "type" (aget d "type")
                                           "show?" (aget d "show?")
                                           "position" (js-obj "x" (.-x pos) "y" (.-y pos))
                                           "inputs" (aget d "inputs")
                                           "outputs" (aget d "outputs"))))))
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
          delete-selected
          (fn []
            (when-let [cell (.-selected ctx)]
              (set! (.-loading ctx) true)
              (.removeCell graph-instance cell)
              (set! (.-selected ctx) nil)
              (set! (.-loading ctx) false)
              (refresh-size!)
              (emit-graph!)))
          fit (fn []
                (.zoomToFit graph-instance (js-obj "padding" 40 "maxScale" 1))
                (set! (.-scale ctx) (.zoom graph-instance))
                (refresh-size!))
          zoom-by (fn [delta]
                    (let [s (js/Math.max 0.2 (js/Math.min 4 (+ (or (.-scale ctx) 1) delta)))]
                      (set! (.-scale ctx) s)
                      (.zoomTo graph-instance s)
                      (refresh-size!)))
          toolbar-click
          (fn [evt]
            (let [btn (.closest (.-target evt) "[data-cmd]")]
              (when btn
                (let [cmd (.-cmd (.-dataset btn))]
                  (cond
                    (= cmd "delete") (delete-selected)
                    (= cmd "zoom-in") (zoom-by 0.1)
                    (= cmd "zoom-out") (zoom-by -0.1)
                    (= cmd "fit") (fit))))))
          key-handler
          (fn [evt]
            (when (and (not (typing? evt)) (= (.-key evt) "Delete"))
              (.preventDefault evt)
              (delete-selected)))
          rebuild
          (fn [g]
            (set! (.-loading ctx) true)
            (set! (.-selected ctx) nil)
            (build-graph! g)
            (set! (.-loading ctx) false)
            (refresh-size!))]
      (set! (.-graph ctx) graph-instance)
      (set! (.-serialize ctx) serialize)
      (set! (.-resizeFn ctx) refresh-size!)
      (set! (.-lastW ctx) nil)
      (set! (.-lastH ctx) nil)
      (set! (.-lastScale ctx) nil)
      (set! (.-pendingR ctx) false)
      (set! (.-selected ctx) nil)
      (set! (.-loading ctx) true)
      (set! (.-scale ctx) 1)
      (set! (.-rev ctx) graph-rev)
      (set! (.-container ctx) container)
      (set! (.-toolbarClick ctx) toolbar-click)
      (set! (.-rebuild ctx) rebuild)
      (when container
        (.addEventListener container "click" toolbar-click))
      (.addEventListener host "click"
                         (fn [evt]
                           (when-not (typing? evt)
                             (let [view (.findViewByElem graph-instance (.-target evt))]
                               (select-cell (when view (.-cell view)))))))
      (.on graph-instance "edge:connected" (fn [_] (emit-graph!)))
      (.on graph-instance "edge:removed" (fn [_] (emit-graph!)))
      (.on graph-instance "node:removed" (fn [_] (emit-graph!)))
      (.on graph-instance "node:moved" (fn [_] (emit-graph!)))
      (build-graph! graph)
      (set! (.-loading ctx) false)
      (refresh-size!)
      (js/setTimeout refresh-size! 60)
      (when (exists? js/window)
        (.addEventListener js/window "resize" refresh-size!)
        (set! (.-resizeHandler ctx) refresh-size!))
      (.addEventListener js/document "keydown" key-handler)
      (set! (.-keyHandler ctx) key-handler)
      (when (exists? js/ResizeObserver)
        (let [ro (new js/ResizeObserver (fn [_] (refresh-size!)))]
          (set! (.-ro ctx) ro)
          (.observe ro wrap)))))
  (update [_root]
    (when (and (not (nil? graph-rev)) (not= graph-rev (.-rev ctx)))
      (set! (.-rev ctx) graph-rev)
      (when-let [rebuild (.-rebuild ctx)]
        (rebuild graph))))
  (unmount [_root]
    (when-let [ro (.-ro ctx)]
      (.disconnect ro))
    (when-let [rh (.-resizeHandler ctx)]
      (.removeEventListener js/window "resize" rh))
    (when-let [g (.-graph ctx)]
      (.dispose g))
    (when-let [kh (.-keyHandler ctx)]
      (.removeEventListener js/document "keydown" kh))
    (when-let [tc (.-toolbarClick ctx)]
      (when-let [c (.-container ctx)]
        (.removeEventListener c "click" tc)))
    (set! (.-graph ctx) nil)
    (set! (.-ro ctx) nil)
    (set! (.-resizeHandler ctx) nil)
    (set! (.-keyHandler ctx) nil)
    (set! (.-toolbarClick ctx) nil)
    (set! (.-container ctx) nil)))

;; ---------------------------------------------------------------------------
;; popup (edit a node's code or option-ui)
;; ---------------------------------------------------------------------------

(defn- open-popup! [r node-id type]
  (let [{:keys [this popup-a code-a rows-a]} r
        node (rui/node-by-id this node-id)]
    (reset! code-a (or (:code node) ""))
    (reset! rows-a (vec (or (:option-ui node) [])))
    (reset! popup-a {:id node-id :label (:label node) :type type})))

(defn- close-popup! [r]
  (let [{:keys [this popup-a code-a rows-a]} r
        {:keys [id type]} @popup-a]
    (when id
      (if (= "formula" (name (keyword (or type "value"))))
        (rui/set-node-code this id @code-a)
        (rui/set-node-option-ui this id @rows-a)))
    (reset! popup-a nil)))

(defn- popup-view [r]
  (let [{:keys [popup-a code-a rows-a]} r
        {:keys [id label type]} @popup-a
        formula? (= "formula" (name (keyword (or type "value"))))]
    [:div {:style "position:fixed;inset:0;background:rgba(17,24,39,0.45);z-index:1000;display:flex;align-items:center;justify-content:center;padding:1rem"
           :data-on:click (h/action (close-popup! r))}
     [:div {:style "background:white;border-radius:0.5rem;box-shadow:0 10px 30px rgba(0,0,0,0.2);width:min(820px,100%);max-height:92vh;display:flex;flex-direction:column;gap:0.6rem;padding:0.85rem"
            :data-on:click (h/expr (.stopPropagation evt))}
      [:div {:style "font-weight:600;font-size:0.85rem;font-family:monospace"}
       (str "Edit " (or label id) (if formula? " (:formula)" " (:value)"))]
      (if formula?
        (input-cm/codemirror-ui code-a {:style {:height "440px"}})
        (input-opt/option-input rows-a))
      [:div {:style "display:flex;justify-content:flex-end;gap:0.5rem"}
       [:button {:type "button"
                 :data-on:click (h/action (close-popup! r))
                 :style "font-size:0.8rem;padding:0.3rem 0.8rem;border:1px solid #2563eb;background:#2563eb;color:white;border-radius:0.375rem;cursor:pointer;font-weight:600"}
        "Close"]]]]))

(defn graph-editor
  "Toolbar, canvas and node-edit popup for one model runner handle.

   Owns every server action the itera-model-graph-editor component emits:
   graph-change and edit.  It also owns the direct toolbar actions: Add
   (:value / :formula) runs itera.runner.ui/add-node! and Save runs
   itera.runner.ui/save-model-edn!.  The toolbar is a sibling of the
   component, not a child; its other buttons (delete / zoom-in / zoom-out /
   fit) carry :data-cmd and are delegated to the component's client-side
   commands.  `graph` nodes must not include :code, :value or :option-ui —
   the edit popup resolves those server-side by node id."
  [this]
  (let [base    [:itera-model (:model-name this) :graph-editor]
        popup-a (h/tab-cursor (conj base :popup) nil)
        code-a  (h/tab-cursor (conj base :code) "")
        rows-a  (h/tab-cursor (conj base :rows) [])
        r       {:this this :popup-a popup-a :code-a code-a
                 :rows-a rows-a}]
    [:div {:class "itera-graph-editor"
           :style "display:flex;flex-direction:column;height:100%;min-height:0;font-family:system-ui,-apple-system,'Segoe UI',sans-serif"}
     [:div {:class "itera-model-toolbar"
            :style "display:flex;flex-wrap:wrap;align-items:center;gap:0.35rem;padding:0.4rem 0.5rem;border-bottom:1px solid #e2e8f0;background:#fff;flex:0 0 auto"}
      [:button {:type "button" :title "Add a value node"
                :data-on:click (h/action (rui/add-node! this :value))
                :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
       "Add :value"]
      [:button {:type "button" :title "Add a formula node"
                :data-on:click (h/action (rui/add-node! this :formula))
                :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
       "Add :formula"]
      [:button {:type "button" :title "Delete the selected node or edge" :data-cmd "delete"
                :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #fca5a5;background:#fef2f2;border-radius:4px;cursor:pointer;color:#b91c1c"}
       "Delete"]
      [:span {:style "flex:1"}]
      [:button {:type "button" :title "Zoom out" :data-cmd "zoom-out"
                :style "font-size:0.85rem;line-height:1;padding:0.15rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
       "−"]
      [:button {:type "button" :title "Zoom in" :data-cmd "zoom-in"
                :style "font-size:0.85rem;line-height:1;padding:0.15rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
       "+"]
      [:button {:type "button" :title "Reset the view" :data-cmd "fit"
                :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
       "Fit"]
      [:button {:type "button" :title "Validate and save the model"
                :data-on:click (h/action (rui/save-model-edn! this))
                :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #86efac;background:#f0fdf4;border-radius:4px;cursor:pointer;color:#166534"}
       "Save"]]
     (itera-model-graph-editor
      {:graph (rui/render-graph this)
       :graph-rev (rui/graph-rev this)
       :data-on:graph-change (h/action (rui/apply-graph! this (get $detail :graph)
                                                         (get $detail :graph-rev)))
       :data-on:edit (h/action (open-popup! r (get $detail :node-id) (get $detail :type)))
       :style "display:block;flex:1 1 auto;min-height:0;width:100%"})
     (when @popup-a
       (popup-view r))]))
