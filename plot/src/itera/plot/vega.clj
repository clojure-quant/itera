(ns itera.plot.vega
  "Vega-Lite and Vega viewers via vega-embed, as a Hyper client component.

   The public fns take only the chart `data`:

     (vegalite rows)    ; Vega-Lite chart
     (vega datasets)    ; raw Vega chart

   The records store only the data and the mode (`:vega` or `:vegalite`).
   The editable chart spec is supplied only through
   itera.value.protocol/itera-spec-plot (the model node `:spec`); there is no
   construction-time default spec, so a bare value renders an empty chart.

   The spec is the Vega / Vega-Lite chart spec.  When rendered through
   itera-spec-plot the container `window` {:width <px> :height <px>} is merged
   into the spec's `:width`/`:height` (vega owns its size in the spec, so the
   fixed interact window overrides any `:width`/`:height` set in the spec) and
   the host element is sized from it.  Since Vega's and Vega-Lite's
   `width`/`height` are the plot rectangle, the default `pad` autosize grows
   the canvas around that rectangle -- past the fixed cell.  When a window is
   supplied the `autosize` of both modes is therefore normalized to `fit` with
   `:contains` padding, so the axes and padding fit inside the cell instead of
   being clipped.

   Bound signal controls (`bind`) are rendered by Vega as an HTML form
   (`.vega-bindings`) below the plot rectangle, outside the area autosize
   accounts for.  After the embed resolves, the component measures the form
   and shrinks the view height so that bindings band fits inside the fixed
   cell instead of overflowing it.

   Data attachment:
     - a map name -> raw value is injected into a raw Vega spec's matching
       `:data` entries as `:values` (the entry's `:url` is removed, `:format`
       is kept so Vega still parses the value);
     - for `:vegalite` a map name -> rows becomes the spec's `:datasets`
       (the spec then references it with {:data {:name ...}});
     - sequential rows are attached as the spec's `:data.values` for
       `:vegalite` only; raw Vega data should be inline in the spec or a
       named-dataset map.

   Data may also come from the spec's own `:url` entries.  The client embed is
   given an HTTP loader with file access disabled, so the esm.sh vega-loader
   build fetches relative URLs such as `/data/x.json` over HTTP instead of
   trying to read them from the filesystem.

   `vegalite` renders a Vega-Lite spec; `vega` forces vega-embed's \"vega\" mode
   for a raw Vega spec.

   The returned `Vega` record implements both itera.value.protocol (renders the
   data with an empty spec) and itera.value.protocol (`:spec` as the editable
   spec)."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.value.protocol :as spec-plot]))

(def head
  [[:style
    ".vega-embed canvas { display: block; }
     .vega-bindings { margin: 0; padding: 4px 0; display: flex; flex-wrap: wrap; gap: .5rem 1rem; font: 12px sans-serif; }
     .vega-bind { display: flex; align-items: center; gap: .35rem; }
     .vega-bind input[type=range] { width: 120px; }"]])

(defn- fitted-plot-height
  "Target canvas height that leaves `bindings-height` px of the window for the
   signal-binding form rendered below it.  nil when there is no box height or
   no non-zero bindings band (so the caller leaves the view alone).

   Pure reference rule for the fit applied by the embedded component; Hyper
   compiles each lifecycle segment on its own, so the component inlines the
   same arithmetic instead of calling this fn.  The component additionally
   subtracts the canvas/height-signal delta that autosize \"none\" specs add
   as vertical padding."
  [box-height bindings-height]
  (when (and (number? box-height) (pos? box-height)
             (number? bindings-height) (pos? bindings-height))
    (max 1 (- box-height bindings-height))))

(h/defc itera-vega-embed
  {:require [["https://esm.sh/vega-embed@6.26.0" :as vega-embed]
             ["https://esm.sh/vega-loader@4.5.4" :as vega-loader]]}
  [{:keys [options window]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [el (.querySelector root "div")
          loader (vega-loader/loader)
          embed-opts (.-embedOpts options)]
      (set! (.-fileAccess loader) false)
      (set! (.-loader embed-opts) loader)
      (-> (vega-embed/default el (.-spec options) embed-opts)
          (.then (fn [res]
                   (set! (.-result ctx) res)
                   (let [spec (.-spec options)
                         autosize (:autosize spec)
                         autosize-type (if (map? autosize) (:type autosize) autosize)
                         view (.-view res)
                         form (.querySelector root ".vega-bindings")
                         bh (when (and form (.querySelector form ".vega-bind"))
                              (.-offsetHeight form))
                         box (some-> window :height)
                         plot (when view (.querySelector root ".chart-wrapper canvas"))
                         ;; autosize "none" canvases also include the spec's
                         ;; vertical padding, which the height signal omits.
                         extra (when (and plot (= "none" autosize-type))
                                 (- (.-offsetHeight plot) (.height view)))]
                     ;; Vega renders the bound-signal controls below the plot
                     ;; rectangle; shrink the view so that band stays inside
                     ;; the fixed cell.  Mirrors fitted-plot-height.
                     (when (and view (number? box) (pos? box)
                                (number? bh) (pos? bh))
                       (.height view (max 1 (- (max 1 (- box bh)) (or extra 0))))
                       (.resize view)
                       (.runAsync view)))))
          (.catch (fn [err] (js/console.error "vega-embed error" err))))))
  (update [root]
    (when-let [r (.-result ctx)]
      (let [v (.-view r)]
        (when v (.finalize v))))
    (set! (.-result ctx) nil)
    (let [el (.querySelector root "div")
          loader (vega-loader/loader)
          embed-opts (.-embedOpts options)]
      (set! (.-fileAccess loader) false)
      (set! (.-loader embed-opts) loader)
      (-> (vega-embed/default el (.-spec options) embed-opts)
          (.then (fn [res]
                   (set! (.-result ctx) res)
                   (let [spec (.-spec options)
                         autosize (:autosize spec)
                         autosize-type (if (map? autosize) (:type autosize) autosize)
                         view (.-view res)
                         form (.querySelector root ".vega-bindings")
                         bh (when (and form (.querySelector form ".vega-bind"))
                              (.-offsetHeight form))
                         box (some-> window :height)
                         plot (when view (.querySelector root ".chart-wrapper canvas"))
                         extra (when (and plot (= "none" autosize-type))
                                 (- (.-offsetHeight plot) (.height view)))]
                     (when (and view (number? box) (pos? box)
                                (number? bh) (pos? bh))
                       (.height view (max 1 (- (max 1 (- box bh)) (or extra 0))))
                       (.resize view)
                       (.runAsync view)))))
          (.catch (fn [err] (js/console.error "vega-embed error" err))))))
  (unmount [_]
    (when-let [r (.-result ctx)]
      (let [v (.-view r)]
        (when v (.finalize v))))))

(defn- inject-data
  "For raw Vega specs, replace :url with :values on each :data entry whose
   :name matches a key in DATA (a map of keyword -> raw value)."
  [spec data]
  (if (and (map? data) (sequential? (:data spec)))
    (update spec :data
            (fn [entries]
              (mapv (fn [entry]
                      (let [k (keyword (:name entry))]
                        (if (contains? data k)
                          (-> entry (dissoc :url) (assoc :values (get data k)))
                          entry)))
                    entries)))
    spec))

(defn- window-autosize
  "Vega and Vega-Lite `width`/`height` are the plot rectangle.  itera supplies
   the cell size as width/height, so fit the gutters inside that size instead
   of letting the default pad autosize grow the canvas past the cell.
   Signal-valued autosize and an explicit \"none\" are left alone."
  [autosize]
  (let [type (if (map? autosize) (:type autosize) autosize)
        type (or type "pad")]
    (cond
      (and (map? autosize) (:signal autosize)) autosize
      (= "none" type) autosize
      (#{"pad" "fit" "fit-x" "fit-y"} type)
      (-> (if (map? autosize) autosize {})
          (assoc :type (if (= "pad" type) "fit" type)
                 :contains "padding"))
      :else autosize)))

(defn- apply-window-autosize
  "When WINDOW supplies the cell size, normalize the spec's autosize so the
   axes and padding fit inside the cell instead of growing the canvas past it.
   Vega and Vega-Lite both treat width/height as the plot rectangle; their
   default \"pad\" autosize adds the gutters outside it.  Explicit \"none\" and
   signal-valued autosize are left alone (see `window-autosize`)."
  [spec window]
  (if (and window (:width window) (:height window))
    (assoc spec :autosize (window-autosize (:autosize spec)))
    spec))

(defn- embed-options
  "Build the itera-vega-embed `options` map from DATA, MODE, CHART-SPEC and
   WINDOW.  DATA is rows/datasets only; the chart spec comes from the caller."
  [data mode chart-spec window]
  (let [spec (merge (if (map? chart-spec) chart-spec {})
                    (select-keys window [:width :height]))
        base-embed (cond-> {}
                     (= mode :vega) (assoc :mode "vega"))
        spec (apply-window-autosize spec window)
        spec (if (= mode :vega) (inject-data spec data) spec)
        [spec embed-opts]
        (cond
          (and (= mode :vegalite) (map? data))
          [(assoc spec :datasets data) base-embed]

          (and (= mode :vegalite) (sequential? data))
          [(assoc spec :data {:values (vec data)}) base-embed]

          :else
          [spec base-embed])]
    {:spec spec :embedOpts embed-opts}))

(defn- render
  "Build the itera-vega-embed element.  STYLE is derived from WINDOW only."
  [data mode chart-spec window]
  (let [style (some-> window spec-plot/window->style)]
    (itera-vega-embed
     (cond-> {:options (embed-options data mode chart-spec window)}
       window (assoc :window window)
       style  (assoc :style style)))))

(defrecord Vega [data mode]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] (render data mode nil nil))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec window] (render data mode spec window))
  (default-spec [_] nil)
  (spec-editor [_ _spec code-a]
    (input-cm/codemirror-ui code-a {:style {:height "440px"}})))

(defn vegalite [data] (->Vega data :vegalite))
(defn vega     [data] (->Vega data :vega))
