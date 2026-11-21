(ns itera.plot.rtable-default-column-formatter
  "Built-in `format-rtable-col` methods for itera.plot.rtable.

   These methods are registered process-globally as soon as an rtable viewer
   loads this namespace, so a table renders its default formats without any
   project setup.  Built in: :nr-format-auto, :prct and :format-date, plus the
   :default fallback.

   Projects register additional formats (typically from a model's :ns-data)
   with (defmethod rtable-fmt/format-rtable-col :my-format ...); the rtable
   spec editor lists every registered keyword method in its dropdown."
  (:require [clojure.string :as str]
            [itera.plot.rtable-format-ui :as rtable-fmt])
  (:import (java.time Instant ZoneOffset)
           (java.time.format DateTimeFormatter)
           (java.time.temporal TemporalAccessor)))

(defn default-format
  "Fallback cell formatting: booleans become check/cross, anything else is
   returned unchanged."
  [value]
  (if (boolean? value)
    (case value true "\u2713" false "\u2715" "")
    value))

(defn- format-number
  "Format VALUE with the given java format string.  nil -> \"-\", strings pass
   through unchanged."
  [fmt value]
  (cond
    (nil? value)    "-"
    (string? value) value
    (number? value) (format fmt (double value))
    :else           (str value)))

(defmethod rtable-fmt/format-rtable-col :nr-format-auto
  [_opts _ value]
  (if (number? value)
    (let [v (Math/abs (double value))]
      (format-number (cond
                       (> v 10000) "%.0f"
                       (> v 1000)  "%.1f"
                       (> v 100)   "%.2f"
                       (> v 10)    "%.3f"
                       (> v 1)     "%.4f"
                       :else       "%.5f")
                     value))
    (format-number "%.0f" value)))

(defmethod rtable-fmt/format-rtable-col :prct
  [_opts _ value]
  (cond
    (nil? value)    "-"
    (string? value) value
    (number? value) (format "%.2f%%" (* 100.0 (double value)))
    :else           (str value)))

(defmethod rtable-fmt/format-rtable-col :format-date
  [_opts opts value]
  (cond
    (nil? value)    ""
    (string? value) value
    :else
    (let [pattern (if (and (string? opts) (not (str/blank? opts)))
                    opts
                    "yyyy-MM-dd")]
      (try
        (let [fmt (DateTimeFormatter/ofPattern pattern)]
          (cond
            (instance? Instant value)
            (.format fmt (.atZone ^Instant value ZoneOffset/UTC))

            (instance? TemporalAccessor value)
            (.format fmt ^TemporalAccessor value)

            :else (str value)))
        (catch Exception _ (str value))))))

(defmethod rtable-fmt/format-rtable-col :default
  [_opts _ value]
  (default-format value))