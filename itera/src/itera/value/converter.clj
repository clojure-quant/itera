(ns itera.value.converter
  "Public value → hiccup entry point.

   Requiring this namespace loads itera.value.clj-types, registering the
   built-in Clojure/Java conversions."
  (:require [itera.value.clj-types :as clj-types]))

(defn type->hiccup
  "Convert any value to server-side hiccup via itera.value.protocol."
  [v]
  (clj-types/value->hiccup v))