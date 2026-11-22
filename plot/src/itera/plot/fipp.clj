(ns itera.plot.fipp
  "Pretty-printed EDN viewer (pure hiccup, no JS).

   The value is pretty-printed on the server with fipp and wrapped in a
   `<pre>` block:

     (fipp {:a 1 :b [2 3]})

   The record stores only the data; its editable spec is a map of
   fipp.edn/pprint options:

     {:width 70 :print-length nil :print-level nil :print-meta false}

   Supported options are :width (positive integer), :print-length and
   :print-level (non-negative integers, absent means unlimited), and
   :print-meta (boolean).  Unknown keys are ignored when rendering.  A nil
   spec uses the defaults; a non-map spec renders an error block."
  (:require [fipp.edn :as fipp-edn]
            [itera.plot.fipp-format-ui :as fipp-fmt]
            [itera.value.protocol :as spec-plot]))

(def head [])

(def default-style
  {:background    "#d0d6fd"
   :margin        "0.5rem"
   :border-radius "0.375rem"
   :overflow      "auto"})

(defn- formatted-style
  "Default style sized for the interact cell: full width, no margin, and the
   window height as a max-height so a tall value scrolls inside the pre."
  [window]
  (let [window-style (spec-plot/window->style window)
        style (merge default-style
                     {:width       "100%"
                      :margin      "0"
                      :box-sizing  "border-box"})]
    (if-let [height (:height window-style)]
      (assoc style :max-height height)
      style)))

(defn render
  "Render DATA as a `<pre>` using OPTIONS (fipp.edn/pprint options), STYLE and
   CLASS for the element.  Pprint failures render an error message in place."
  [data {:keys [options style class]}]
  [:pre (cond-> {}
          class (assoc :class class)
          style (assoc :style style))
   (try
     (with-out-str (fipp-edn/pprint data options))
     (catch Exception e
       (str "fipp error: " (.getMessage e))))])

(defrecord Fipp [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_]
    (render data {:options fipp-fmt/default-options
                  :style   default-style}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (if-let [options (fipp-fmt/normalize-options spec)]
      (render data {:options options
                    :style   (formatted-style window)})
      [:div {:style {:padding "1rem" :color "#888"}}
       "Invalid fipp spec: expected a map of fipp.edn/pprint options"]))
  (default-spec [_]
    fipp-fmt/default-options)
  (spec-editor [_ spec code-a]
    (fipp-fmt/fipp-format-edit-ui spec code-a)))

(defn fipp
  "Pretty-print `data` with fipp and return it as an `itera-convertable`
   record.

   The editable spec is passed, after sanitation, verbatim to
   `(fipp.edn/pprint data options)`:
   {:width 70 :print-length nil :print-level nil :print-meta false}.
   `:width` is a positive integer; `:print-length` / `:print-level` are
   non-negative integers or absent for unlimited; `:print-meta` is a boolean."
  [data]
  (->Fipp data))