(ns itera.value.converter
  "Public value → hiccup entry point.

   Requiring this namespace loads itera.value.clj-types, registering the
   built-in Clojure/Java conversions."
  (:require [itera.value.protocol :as protocol]
            [itera.value.clj-types :as clj-types])
  (:import [itera.value.protocol itera_convertable]))

(defn type->hiccup
  "Convert any value to server-side hiccup via itera.value.protocol."
  [v]
  (clj-types/value->hiccup v))

(defn render-tree
  "Resolve directly-implemented itera-convertable records inside a rendered
   hiccup tree. Hiccup elements are traversed; other values pass through."
  [v]
  (cond
    (instance? itera_convertable v)
    (render-tree (protocol/to-hiccup v))

    (and (vector? v) (keyword? (first v)))
    (let [[tag & body] v
          head   (first body)
          attrs? (and (map? head) (not (record? head)))
          attrs  (when attrs? head)]
      (into [tag] (concat (when attrs? [attrs])
                          (map render-tree (if attrs? (rest body) body)))))

    (sequential? v) (mapv render-tree v)
    :else v))