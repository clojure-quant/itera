(ns itera.plot.cytoscape
  "Cytoscape (vanilla JS) graph viewer as a Hyper client component.

   The public `cytoscape` fn splits host-element passthrough (`:style`,
   `:class`) from the Cytoscape init payload.

   opts:
     :elements   node/edge elements (required)
     :stylesheet Cytoscape stylesheet vector (mapped to Cytoscape's `style`)
     :layout     layout map, e.g. {:name \"dagre\"} or {:name \"cose-bilkent\"}
     :style      host element CSS (NOT the Cytoscape stylesheet)
     :class      host element class
   Any other keys are passed through to Cytoscape's init options.

   Note the `:style` / `:stylesheet` split: Cytoscape itself calls its
   stylesheet `style`, but this viewer reserves `:style` for the host
   element. `:stylesheet` is translated to Cytoscape's `style` option.

   The `dagre` and `cose-bilkent` layout extensions are registered on mount,
   so `:layout {:name \"dagre\"}` and `:layout {:name \"cose-bilkent\"}` work."
  (:require [hyper.core :as h]))

(def head [])

(h/defc itera-cytoscape
  "Vanilla Cytoscape graph in a custom element. `options` is the JSON-parsed
   Cytoscape init options."
  {:require [["https://esm.sh/cytoscape@3.30.2" :as cy]
             ["https://esm.sh/cytoscape-dagre@2.5.0" :as dagre]
             ["https://esm.sh/cytoscape-cose-bilkent@4.1.0" :as cose-bilkent]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%;position:relative"}])
  (mount [root]
    (let [C cy/default
          el (.querySelector root "div")]
      (.use C dagre/default)
      (.use C cose-bilkent/default)
      (set! (.-container options) el)
      (set! (.-cy ctx) (C options))))
  (update [root]
    ;; First cut: re-create the instance on any attribute change.
    (when-let [c (.-cy ctx)] (.destroy c))
    (let [C cy/default
          el (.querySelector root "div")]
      (.use C dagre/default)
      (.use C cose-bilkent/default)
      (set! (.-container options) el)
      (set! (.-cy ctx) (C options))))
  (unmount [_]
    (when-let [c (.-cy ctx)] (.destroy c))))

(defn cytoscape
  [{:keys [style class stylesheet] :as opts}]
  (itera-cytoscape
   (cond-> {:options (cond-> (dissoc opts :style :class :stylesheet)
                       stylesheet (assoc :style stylesheet))}
     style (assoc :style style)
     class (assoc :class class))))
