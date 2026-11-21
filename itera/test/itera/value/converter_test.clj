(ns itera.value.converter-test
  (:require [clojure.test :refer [are deftest is]]
            [itera.value.converter :as converter]
            [itera.value.protocol :as protocol])
  (:import (java.time Duration Instant LocalDate LocalDateTime LocalTime
                     OffsetDateTime OffsetTime Period ZonedDateTime)))

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

(deftest renders-java-time-values
  (are [v class s] (= [:span {:class class} s] (converter/type->hiccup v))
    (Instant/parse "2026-01-02T03:04:05Z")
    "clj-instant" "2026-01-02T03:04:05Z"

    (LocalDate/of 2026 1 2)
    "clj-localdate" "2026-01-02"

    (LocalDateTime/of 2026 1 2 3 4 5)
    "clj-localdatetime" "2026-01-02T03:04:05"

    (LocalTime/of 3 4 5)
    "clj-localtime" "03:04:05"

    (ZonedDateTime/parse "2026-01-02T03:04:05+01:00[Europe/Paris]")
    "clj-zoneddatetime" "2026-01-02T03:04:05+01:00[Europe/Paris]"

    (OffsetDateTime/parse "2026-01-02T03:04:05+01:00")
    "clj-offsetdatetime" "2026-01-02T03:04:05+01:00"

    (OffsetTime/parse "03:04:05+01:00")
    "clj-offsettime" "03:04:05+01:00"

    (Duration/ofMinutes 90)
    "clj-duration" "PT1H30M"

    (Period/of 1 2 3)
    "clj-period" "P1Y2M3D"))

(deftest renders-instant-inside-collections
  (let [inst (Instant/parse "2026-01-02T03:04:05Z")
        out  (pr-str (converter/type->hiccup {:when inst :xs [inst]}))]
    (is (not (re-find #"unknown type" out)))
    (is (= 2 (count (re-seq #"2026-01-02T03:04:05Z" out))))))