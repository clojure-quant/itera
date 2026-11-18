(ns itera.model-ui.graph-editor
  "AntV X6 model-graph editor as a Hyper client component.

   The model-page graph editor: node types are
   just :value and :formula, ports come from the server (formula :args), node
   boxes expose an editable label, an edit box and a show/hide eye, and the
   graph area scrolls (outer overflow:auto over a content-sized X6 host) with
   zoom in/out buttons.

   Graph nodes carry :id (the X6 cell id, an immutable server-generated
   nano-id) and :label (displayed/editable, may repeat).  Creating a node is
   server-owned: the Add buttons emit :add-node and the server replies with a
   rebuilt graph.

   Attrs:
     graph        - server-rendered graph {:nodes [...] :edges [...]} where
                    each node's :id is the X6 cell id and :label is the display
                    name, plus ports
     graph-rev    - server counter; a change forces the client to rebuild from
                    `graph` (used when ports change after a code edit and after
                    a server-created node)
     save-result  - {:status \"ok\"|\"error\"} from the last save action
     data-on:graph-change - server action; detail {:graph <serialized>
                                                   :graph-rev <rev>}
     data-on:add-node     - server action; detail {:type \"value\"|\"formula\"}
     data-on:edit         - server action; detail {:node-id <id> :type <type>}
     data-on:save         - server action (no detail)

   The client edits the graph locally and emits the full serialized graph on
   every structural change (delete/connect/disconnect/drag/show?/label), so the
   server can reconcile without the client rebuilding on its own echo."
  (:require [hyper.core :as h]))

(h/defc itera-model-graph-editor
  "X6 model-graph editor web component."
  {:require [["https://esm.sh/@antv/x6@3.1.8?bundle" :as x6]]}
  [{:keys [graph graph-rev save-result]}]
  (render
   [:div {:style "display:flex;flex-direction:column;height:100%;min-height:0;font-family:system-ui,-apple-system,'Segoe UI',sans-serif"}
    [:div {:class "itera-model-toolbar"
           :style "display:flex;flex-wrap:wrap;align-items:center;gap:0.35rem;padding:0.4rem 0.5rem;border-bottom:1px solid #e2e8f0;background:#fff;flex:0 0 auto"}
     [:button {:type "button" :title "Add a value node"
               :on {:click (fn [_] ((.-add-value ctx)))}
               :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
      "Add :value"]
     [:button {:type "button" :title "Add a formula node"
               :on {:click (fn [_] ((.-add-formula ctx)))}
               :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
      "Add :formula"]
     [:button {:type "button" :title "Delete the selected node or edge"
               :on {:click (fn [_] ((.-delete-selected ctx)))}
               :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #fca5a5;background:#fef2f2;border-radius:4px;cursor:pointer;color:#b91c1c"}
      "Delete"]
     [:span {:style "flex:1"}]
     [:button {:type "button" :title "Zoom out"
               :on {:click (fn [_] ((.-zoom-out ctx)))}
               :style "font-size:0.85rem;line-height:1;padding:0.15rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
      "−"]
     [:button {:type "button" :title "Zoom in"
               :on {:click (fn [_] ((.-zoom-in ctx)))}
               :style "font-size:0.85rem;line-height:1;padding:0.15rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
      "+"]
     [:button {:type "button" :title "Reset the view"
               :on {:click (fn [_] ((.-fit ctx)))}
               :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #cbd5e1;background:#f8fafc;border-radius:4px;cursor:pointer;color:#334155"}
      "Fit"]
     [:span {:class "itera-model-status"
             :style "font-size:0.7rem;color:#475569;white-space:nowrap;min-width:5rem;text-align:right"}
      "Saved"]
     [:button {:type "button" :title "Validate and save the model"
               :on {:click (fn [_] ((.-save ctx)))}
               :style "font-size:0.7rem;padding:0.2rem 0.5rem;border:1px solid #86efac;background:#f0fdf4;border-radius:4px;cursor:pointer;color:#166534"}
      "Save"]]
    [:div {:class "itera-model-scroll"
           :style "flex:1;min-height:0;overflow:auto;position:relative;background:#f8fafc"}
     [:div {:class "itera-model-canvas"
            :style "position:relative;width:100%;height:100%;overflow:hidden"}]]
    [:style ".itera-model-canvas .x6-graph{width:100%;height:100%}.itera-model-canvas foreignObject>body{margin:0;padding:0;overflow:visible}"]])
  (mount [root]
    (let [Shape (.-HTML (.-Shape x6))
          wrap (.querySelector root ".itera-model-scroll")
          host (.querySelector root ".itera-model-canvas")
          status (.querySelector root ".itera-model-status")
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
          set-status (fn [text color]
                       (when status
                         (set! (.-textContent status) text)
                         (set! (.-color (.-style status)) color)))
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
                        (when-not (or (.-loading ctx) (.-adding ctx))
                          (let [ser ((.-serialize ctx))
                                sig (js/JSON.stringify ser)]
                            (when (not= sig (.-lastSig ctx))
                              (set! (.-lastSig ctx) sig)
                              (set-status "Unsaved changes" "#b45309")
                              (emit "graph-change"
                                    (js-obj "graph" ser
                                            "graph-rev" (.-rev ctx)))))))
          clone-data (fn [d overrides]
                       (let [o (js-obj)]
                         (.forEach (array "label" "type" "show?" "inputs"
                                          "outputs" "code" "value" "optionUi")
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
                                                          "outputs" outs
                                                          "code" (aget n "code")
                                                          "value" (aget n "value")
                                                          "optionUi" (aget n "option-ui"))
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
                                           "outputs" (aget d "outputs")
                                           "code" (aget d "code")
                                           "value" (aget d "value")
                                           "option-ui" (aget d "optionUi"))))))
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
          request-add-node
          (fn [type-kw]
            (when-not (.-adding ctx)
              (set! (.-adding ctx) true)
              (set-status "Adding..." "#475569")
              (emit "add-node" (js-obj "type" type-kw))))
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
          save (fn []
                 (set-status "Saving..." "#475569")
                 (emit "save" (js-obj)))
          key-handler
          (fn [evt]
            (when (and (not (typing? evt)) (= (.-key evt) "Delete"))
              (.preventDefault evt)
              (delete-selected)))
          rebuild
          (fn [g]
            (set! (.-adding ctx) false)
            (set! (.-loading ctx) true)
            (set! (.-selected ctx) nil)
            (build-graph! g)
            (set! (.-loading ctx) false)
            (set-status "Unsaved changes" "#b45309")
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
      (set! (.-adding ctx) false)
      (set! (.-scale ctx) 1)
      (set! (.-rev ctx) graph-rev)
      (set! (.-add-value ctx) (fn [] (request-add-node "value")))
      (set! (.-add-formula ctx) (fn [] (request-add-node "formula")))
      (set! (.-delete-selected ctx) delete-selected)
      (set! (.-fit ctx) fit)
      (set! (.-zoom-in ctx) (fn [] (zoom-by 0.1)))
      (set! (.-zoom-out ctx) (fn [] (zoom-by -0.1)))
      (set! (.-save ctx) save)
      (set! (.-rebuild ctx) rebuild)
      (set! (.-statusEl ctx) status)
      (set! (.-setStatus ctx) set-status)
      (set! (.-saveStatus ctx) nil)
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
      (set-status "Saved" "#475569")
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
    (let [sr (when save-result (js/JSON.stringify save-result))
          save-status (when save-result (aget save-result "status"))]
      (when (and save-status (not= sr (.-saveStatus ctx)))
        (set! (.-saveStatus ctx) sr)
        (if (= save-status "ok")
          ((.-setStatus ctx) "Saved" "#475569")
          ((.-setStatus ctx) "Save failed" "#b91c1c")))
      (when (and (not (nil? graph-rev)) (not= graph-rev (.-rev ctx)))
        (set! (.-rev ctx) graph-rev)
        (when-let [rebuild (.-rebuild ctx)]
          (rebuild graph)))))
  (unmount [_root]
    (when-let [ro (.-ro ctx)]
      (.disconnect ro))
    (when-let [rh (.-resizeHandler ctx)]
      (.removeEventListener js/window "resize" rh))
    (when-let [g (.-graph ctx)]
      (.dispose g))
    (when-let [kh (.-keyHandler ctx)]
      (.removeEventListener js/document "keydown" kh))
    (set! (.-graph ctx) nil)
    (set! (.-ro ctx) nil)
    (set! (.-resizeHandler ctx) nil)
    (set! (.-keyHandler ctx) nil)))
