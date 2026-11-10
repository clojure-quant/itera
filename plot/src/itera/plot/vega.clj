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
   for a raw Vega spec."
  (:require [hyper.core :as h]))

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

(defn vegalite
  [{:keys [style class] :as opts}]
  (itera-vega-embed (cond-> {:options (payload opts)}
                      style (assoc :style style)
                      class (assoc :class class))))

(defn vega
  [{:keys [style class] :as opts}]
  (let [p (payload opts)
        p (assoc p :embedOpts (assoc (:embedOpts p) :mode "vega"))]
    (itera-vega-embed (cond-> {:options p}
                        style (assoc :style style)
                        class (assoc :class class)))))
