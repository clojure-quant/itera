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

(defn render [data {:keys [class style] :as opts}]
    [:pre (cond-> {}
           class (assoc :class class)
           style (assoc :style style))
    (with-out-str (fipp-edn/pprint data))])

(defrecord Fipp [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] 
    (render data {:style {:background "#d0d6fd"
                          :margin "0.5rem"
                          :border-radius "0.375rem"
                          :overflow "auto"}})))


(defn fipp
  "Pretty-print `value` with fipp and return it as a `<pre>` hiccup block
   wrapped in an `itera-convertable` record."
  [data]
  (Fipp. data))