(ns itera.value.clj-types
  "Converts Clojure values to hiccup representation.

   Loading this namespace registers the built-in Clojure/Java type
   extensions for `itera.value.protocol/itera-convertable`. Prefer requiring
   `itera.value.converter`, which does this for you."
  (:require [itera.value.protocol :refer [itera-convertable to-hiccup]]))

;; Errors and unknown values.

(defn- unknown-type [v]
  [:div {:style {:border "solid 1px red"
                 :padding "4px"}}
   [:p {:style {:color "red"}} "unknown type: " (-> v type str)]
   [:span (pr-str v)]])

(defn- type-convert-err [ex-message]
  [:div {:style {:border "solid 1px red"
                 :padding "4px"}}
   [:p {:style {:color "red"}} "type convert error: " ex-message]])

(defn value->hiccup [v]
  (try
    (if (satisfies? itera-convertable v)
      (to-hiccup v)
      (unknown-type v))
    (catch Exception ex
      (type-convert-err (ex-message ex)))))

;; Render helpers.

(defn- simplevalue->hiccup [v class]
  [:span {:class class}
   (pr-str v)])

(def ^:private delim-style
  {:font-weight "700"
   :color "#0f766e"})

(defn- render-list [{:keys [class open close children]}]
  [:span {:class class}
   [:span {:style (assoc delim-style :margin-right "4px")} open]
   (into [:span]
         (map (fn [child]
                [:span {:style {:margin-right "4px"}} child])
              children))
   [:span {:style (assoc delim-style :margin-left "4px")} close]])

(defn- list->hiccup [opts xs]
  (render-list (assoc opts :children (map value->hiccup xs))))

(defn- map->hiccup [opts m]
  (render-list
   (assoc opts :children (interleave (map value->hiccup (keys m))
                                     (map value->hiccup (vals m))))))

;; Renderers for basic Clojure forms.

;; nil values are a distinct thing of their own
(extend-type nil
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-nil")))

(extend-type Boolean
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-boolean")))

(extend-type clojure.lang.Symbol
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-symbol")))

(extend-type clojure.lang.Namespace
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-namespace")))

(extend-type clojure.lang.Keyword
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-keyword")))

(extend-type clojure.lang.Var
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-var")))

(extend-type clojure.lang.Atom
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-atom")))

(extend-type clojure.lang.Agent
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-agent")))

(extend-type clojure.lang.Ref
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-ref")))

(extend-type java.lang.Class
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-class")))

;; primitive types

(extend-type java.lang.Character
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-char")))

(extend-type java.lang.String
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-string")))

(extend-type java.lang.Integer
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-int")))

(extend-type java.lang.Long
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-long")))

(extend-type clojure.lang.BigInt
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-bigint")))

(extend-type java.lang.Float
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-float")))

(extend-type java.lang.Double
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-double")))

(extend-type java.math.BigDecimal
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-bigdecimal")))

(extend-type clojure.lang.Ratio
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-ratio")))

;; time

(extend-type java.time.LocalDate
  itera-convertable
  (to-hiccup [v]
    (simplevalue->hiccup v "clj-localdate")))

;; renderers for collection of items

(extend-type clojure.lang.IPersistentVector
  itera-convertable
  (to-hiccup [v]
    (list->hiccup
     {:class "clj-vector"
      :open "["
      :close "]"
      :separator " "}
     v)))

(extend-type clojure.lang.LazySeq
  itera-convertable
  (to-hiccup [v]
    (list->hiccup
     {:class "clj-lazy-seq"
      :open "("
      :close ")"
      :separator " "}
     v)))

(extend-type clojure.lang.IPersistentList
  itera-convertable
  (to-hiccup [v]
    (list->hiccup
     {:class "clj-list"
      :open "("
      :close ")"
      :separator " "}
     v)))

(extend-type clojure.lang.ArraySeq
  itera-convertable
  (to-hiccup [v]
    (list->hiccup
     {:class "clj-list"
      :open "("
      :close ")"
      :separator " "}
     v)))

(extend-type clojure.lang.Cons
  itera-convertable
  (to-hiccup [v]
    (list->hiccup
     {:class "clj-list"
      :open "("
      :close ")"
      :separator " "}
     v)))

(extend-type clojure.lang.LongRange
  itera-convertable
  (to-hiccup [v]
    (list->hiccup
     {:class "clj-list"
      :open "("
      :close ")"
      :separator " "}
     v)))

(extend-type clojure.lang.IPersistentSet
  itera-convertable
  (to-hiccup [v]
    (list->hiccup
     {:class "clj-set"
      :open "#{"
      :close "}"
      :separator " "}
     v)))

(extend-type clojure.lang.IPersistentMap
  itera-convertable
  (to-hiccup [v]
    (map->hiccup
     {:class "clj-map"
      :open "{"
      :close "}"
      :separator " "}
     v)))

;; A record is like a map, but it is tagged with its type
(extend-type clojure.lang.IRecord
  itera-convertable
  (to-hiccup [v]
    (map->hiccup
     {:class "clj-record"
      :open "{"
      :close "}"
      :separator " "
      :type (str "#" (pr-str (type v)))}
     v)))