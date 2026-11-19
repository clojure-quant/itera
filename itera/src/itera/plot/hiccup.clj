(ns itera.plot.hiccup
  "Raw hiccup viewer (pure hiccup, no JS).

   A hiccup-shaped vector (`[keyword ...]`) is returned unchanged. Every
   other value is converted to hiccup via `itera.value.converter`, so plain
   Clojure collections are rendered as data rather than treated as markup.

     (hiccup [:h1 \"Title\"])
     (hiccup {:a 1 :b [2 3]})"
  (:require [itera.value.protocol]))

(def head [])

(defrecord Hiccup [hiccup]
  itera.value.protocol/itera-convertable
  (to-hiccup [v]
    (:hiccup v)))

(defn hiccup [h]
  (Hiccup. h))


