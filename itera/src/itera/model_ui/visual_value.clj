(ns itera.model-ui.visual-value
  "Rendering of model node values for the model UI."
  (:require [hyper.core :as h]))

(defn- hiccup-value? [v]
  (or (string? v)
      (and (vector? v) (keyword? (first v)))))

(defn render-value [a]
  (if a
    (h/reactive [a]
                (let [v @a]
                  (when (hiccup-value? v) v)))
    [:span {:style {:color "#94a3b8"}} "—"]))