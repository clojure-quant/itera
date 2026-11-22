(ns itera.plot.cheetah
  "cheetah-grid (canvas) viewer as a Hyper client component.

   The public `cheetah` fn takes only the row `data`:
     (cheetah [{...} ...])

   Column definitions (the editable spec) come from the host through
   itera.value.protocol and map onto cheetah-grid's ListGrid `header`.
   cheetah-grid does not auto-generate columns, so with no spec given the grid
   renders empty; `default-spec` is `nil`.

   The returned `Cheetah` record implements both itera.value.protocol (renders
   `{:records data}`) and itera.value.protocol (`:columns` as the editable
   spec).

   cheetah-grid ships no stylesheet: its CSS is injected by the JS bundle, so
   `head` is empty."
  (:require [hyper.core :as h]
            [itera.input.codemirror :as input-cm]
            [itera.value.protocol :as spec-plot]))

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

(defn- render
  "Build the itera-cheetah element from an opts map (reference viewer opts plus
   :style/:class host passthrough)."
  [{:keys [columns data style class] :as _opts}]
  (itera-cheetah (cond-> {:options (cond-> {}
                                     columns (assoc :header columns)
                                     data    (assoc :records data))}
                   style (assoc :style style)
                   class (assoc :class class))))

(defrecord Cheetah [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] (render {:data data}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (let [style (spec-plot/window->style window)]
      (render (cond-> {:data data}
                spec  (assoc :columns spec)
                style (assoc :style style)))))
  (default-spec [_] nil)
  (spec-editor [_ _spec code-a]
    (input-cm/codemirror-ui code-a {:style {:height "440px"}})))

(defn cheetah
  [data]
  (->Cheetah data))
