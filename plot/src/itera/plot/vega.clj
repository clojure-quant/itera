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
   the host element is sized from it.

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

(def head [])

(h/defc itera-vega-embed
  {:require [["https://esm.sh/vega-embed@6.26.0" :as vega-embed]
             ["https://esm.sh/vega-loader@4.5.4" :as vega-loader]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [el (.querySelector root "div")
          loader (vega-loader/loader)
          embed-opts (.-embedOpts options)]
      (set! (.-fileAccess loader) false)
      (set! (.-loader embed-opts) loader)
      (-> (vega-embed/default el (.-spec options) embed-opts)
          (.then (fn [res] (set! (.-result ctx) res)))
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
          (.then (fn [res] (set! (.-result ctx) res)))
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

(defn- embed-options
  "Build the itera-vega-embed `options` map from DATA, MODE, CHART-SPEC and
   WINDOW.  DATA is rows/datasets only; the chart spec comes from the caller."
  [data mode chart-spec window]
  (let [spec (merge (if (map? chart-spec) chart-spec {})
                    (select-keys window [:width :height]))
        base-embed (cond-> {}
                     (= mode :vega) (assoc :mode "vega"))
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
       style (assoc :style style)))))

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
