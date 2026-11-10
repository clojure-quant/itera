(ns itera.plot.cheetah
  "cheetah-grid (canvas) viewer as a Hyper client component.

   The public `cheetah` fn accepts the reference viewer API (`:columns` for the
   grid header defs, `:data` for the records) and maps them onto cheetah-grid's
   ListGrid options (`header` / `records`).

   opts:
     :columns  [{:field \"make\" :caption \"Make\"} ...]
     :data     [{:make \"Toyota\" ...} ...]
     :style :class  host passthrough

   cheetah-grid ships no stylesheet: its CSS is injected by the JS bundle, so
   `head` is empty."
  (:require [hyper.core :as h]))

(def head [])

(h/defc itera-cheetah
  "cheetah-grid ListGrid in a custom element."
  {:require [["https://esm.sh/cheetah-grid@1.16.6" :as cg]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [ListGrid (.-ListGrid cg/default)
          el (.querySelector root "div")]
      (set! (.-parentElement options) el)
      (set! (.-grid ctx) (new ListGrid options))))
  (update [root]
    (when-let [g (.-grid ctx)] (.dispose g))
    (let [ListGrid (.-ListGrid cg/default)
          el (.querySelector root "div")]
      (set! (.-parentElement options) el)
      (set! (.-grid ctx) (new ListGrid options))))
  (unmount [_]
    (when-let [g (.-grid ctx)] (.dispose g))))

(defn cheetah
  [{:keys [columns data style class] :as _opts}]
  (itera-cheetah (cond-> {:options (cond-> {}
                                    columns (assoc :header columns)
                                    data    (assoc :records data))}
                   style (assoc :style style)
                   class (assoc :class class))))
