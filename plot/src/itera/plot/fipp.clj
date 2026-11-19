(ns itera.plot.fipp
  "Pretty-printed EDN viewer (pure hiccup, no JS).

   The value is pretty-printed on the server with fipp and wrapped in a
   `<pre>` block:

     (fipp {:a 1 :b [2 3]})
     (fipp value {:class \"my-pp\" :style {:max-height \"30rem\"}})

   The optional opts map carries `:class` / `:style` passthrough for the
   `<pre>` element."
  (:require [fipp.edn :as fipp-edn]
            [itera.value.protocol]))

(def head [])

(defrecord Fipp [hiccup]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] hiccup))

(defn fipp
  "Pretty-print `value` with fipp and return it as a `<pre>` hiccup block
   wrapped in an `itera-convertable` record.

   Optional opts map: `{:class <string> :style <map>}` passthrough."
  ([value] (fipp value nil))
  ([value {:keys [class style]}]
   (Fipp.
    [:pre (cond-> {}
            class (assoc :class class)
            style (assoc :style style))
     (with-out-str (fipp-edn/pprint value))])))