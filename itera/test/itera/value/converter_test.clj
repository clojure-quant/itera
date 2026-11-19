(ns itera.value.converter-test
  (:require [clojure.test :refer [deftest is]]
            [itera.value.converter :as converter]
            [itera.value.protocol :as protocol]))

(defrecord View [hiccup]
  protocol/itera-convertable
  (to-hiccup [_] hiccup))

(defrecord Plain [a])

(deftest resolves-direct-record-nested
  (is (= [:div [:span "hi"]]
         (converter/render-tree [:div (->View [:span "hi"])])))
  (is (= [:section {:class "x"} [:p "hi"]]
         (converter/render-tree [:section {:class "x"} (->View [:p "hi"])]))))

(deftest passes-plain-record-through
  (let [v (->Plain 1)]
    (is (satisfies? protocol/itera-convertable v))
    (is (not (instance? itera.value.protocol.itera_convertable v)))
    (is (identical? v (converter/render-tree v)))
    (is (= [:div v] (converter/render-tree [:div v])))))

(deftest preserves-raw-hiccup-and-strings
  (is (= [:div {:class "x"} "text" [:b "bold"]]
         (converter/render-tree [:div {:class "x"} "text" [:b "bold"]])))
  (is (= ["a" "b"] (converter/render-tree ["a" "b"])))
  (is (= "plain" (converter/render-tree "plain")))
  (is (nil? (converter/render-tree nil))))