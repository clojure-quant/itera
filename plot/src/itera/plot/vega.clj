(ns itera.plot.vega
  "Vega-Lite and Vega viewers via vega-embed, as a Hyper client component.

   opts:
     :spec       the Vega-Lite (or Vega) spec map (required)
     :data       optional data, one of:
                   - a vector of rows  -> attached as the spec's :data.values
                   - a map name -> rows -> attached as embed `datasets`
                     (the spec then references it with {:data {:name ...}})
     :embed-opts vega-embed EmbedOptions (e.g. :theme \"quartz\" :width :height)
     :style :class  host passthrough

   `vegalite` renders a Vega-Lite spec; `vega` forces vega-embed's \"vega\" mode
   for a raw Vega spec.

   The returned `Vega` record implements both itera.value.protocol (a prebuilt
   hiccup, byte-identical to the classic viewer) and itera.plot-ui.protocol
   (`:spec` as the editable spec).  When rendered through itera-spec-plot the
   container `window` {:width <px> :height <px>} is injected into the spec's
   `:width`/`:height` (vega owns its size in the spec, so the fixed interact
   window overrides any `:width`/`:height` set in the spec); `:style`/`:class`
   stay host passthrough."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.plot-ui.protocol :as spec-plot]
            [itera.value.protocol]))

(def head [])

(h/defc itera-vega-embed
  {:require [["https://esm.sh/vega-embed@6.26.0" :as vega-embed]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [el (.querySelector root "div")]
      (-> (vega-embed/default el (.-spec options) (.-embedOpts options))
          (.then (fn [res] (set! (.-result ctx) res)))
          (.catch (fn [err] (js/console.error "vega-embed error" err))))))
  (update [root]
    (when-let [r (.-result ctx)]
      (let [v (.-view r)]
        (when v (.finalize v))))
    (set! (.-result ctx) nil)
    (let [el (.querySelector root "div")]
      (-> (vega-embed/default el (.-spec options) (.-embedOpts options))
          (.then (fn [res] (set! (.-result ctx) res)))
          (.catch (fn [err] (js/console.error "vega-embed error" err))))))
  (unmount [_]
    (when-let [r (.-result ctx)]
      (let [v (.-view r)]
        (when v (.finalize v))))))

(defn- payload
  "Normalize viewer opts into the component `options` map."
  [{:keys [spec data embed-opts]}]
  (cond
    (map? data)
    {:spec spec :embedOpts (assoc (or embed-opts {}) :datasets data)}

    (sequential? data)
    {:spec (assoc spec :data {:values (vec data)}) :embedOpts (or embed-opts {})}

    :else
    {:spec spec :embedOpts (or embed-opts {})}))

(defn- render
  "Build the itera-vega-embed element from an opts map plus :mode
   (:vega or :vegalite) and :style/:class host passthrough."
  [{:keys [style class mode] :as opts}]
  (let [p (payload opts)
        p (if (= :vega mode)
            (assoc p :embedOpts (assoc (:embedOpts p) :mode "vega"))
            p)]
    (itera-vega-embed (cond-> {:options p}
                        style (assoc :style style)
                        class (assoc :class class)))))

(defrecord Vega [hiccup opts spec]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] hiccup)
  spec-plot/itera-spec-plot
  (to-hiccup [_ spec window]
    (render (assoc opts
                   :spec (merge spec (select-keys window [:width :height]))
                   :style (merge (:style opts) (spec-plot/window->style window)))))
  (default-spec [_] spec)
  (spec-editor [_ _spec code-a]
    (input-cm/codemirror-ui code-a {:style {:height "440px"}})))

(defn vegalite [opts]
  (->Vega (render (assoc opts :mode :vegalite)) (assoc opts :mode :vegalite) (:spec opts)))

(defn vega [opts]
  (->Vega (render (assoc opts :mode :vega)) (assoc opts :mode :vega) (:spec opts)))
