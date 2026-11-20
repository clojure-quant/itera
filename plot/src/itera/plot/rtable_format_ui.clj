(ns itera.plot.rtable-format-ui
  "Column formatting methods and a table editor for an rtable spec.

   `format-rtable-col` formats a cell value; it dispatches on a column's
   :format keyword and receives the column's :format-opts string.  Built in:
   :prct, :nr-format-auto and :format-date.  `style-rtable-col` returns extra
   cell attrs {:class .. :style ..} for a value and dispatches on :style-fn.
   Both multimethods have a :default method, so a nil/unknown keyword falls
   back to plain formatting and no extra attrs.

   Viewer projects register their own methods (typically from a model's
   :ns-data) with (defmethod format-rtable-col :my-format ...) and
   (defmethod style-rtable-col :my-style ...); `rtable-format-edit-ui` lists
   every registered keyword method in its dropdowns.

   `rtable-format-edit-ui` is the rtable spec editor: one table row per spec
   entry, editing :path / :width / :format / :format-opts / :style-fn.  All
   edits are written immediately as EDN into the caller's code cursor, so the
   existing spec popup persists them when it closes.

   Must be called inside a Hyper render context."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hyper.core :as h])
  (:import (java.time Instant ZoneOffset)
           (java.time.format DateTimeFormatter)
           (java.time.temporal TemporalAccessor)))

;; ---------------------------------------------------------------------------
;; formatting
;; ---------------------------------------------------------------------------

(defn default-format
  "Fallback cell formatting: booleans become check/cross, anything else is
   returned unchanged."
  [value]
  (if (boolean? value)
    (case value true "\u2713" false "\u2715" "")
    value))

(defmulti format-rtable-col
  "Format VALUE for a column.  Dispatches on the :format keyword; OPTS is the
   column's :format-opts string (may be nil)."
  (fn [fmt _opts _value] fmt))

(defmulti style-rtable-col
  "Return cell attrs {:class .. :style ..} for VALUE.  Dispatches on the
   :style-fn keyword; nil/{} means no extra attrs."
  (fn [style-fn _value] style-fn))

(defn- format-number
  "Format VALUE with the given java format string.  nil -> \"-\", strings pass
   through unchanged."
  [fmt value]
  (cond
    (nil? value)    "-"
    (string? value) value
    (number? value) (format fmt (double value))
    :else           (str value)))

(defmethod format-rtable-col :nr-format-auto
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

(defmethod format-rtable-col :prct
  [_opts _ value]
  (cond
    (nil? value)    "-"
    (string? value) value
    (number? value) (format "%.2f%%" (* 100.0 (double value)))
    :else           (str value)))

(defmethod format-rtable-col :format-date
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

(defmethod format-rtable-col :default
  [_opts _ value]
  (default-format value))

(defmethod style-rtable-col :default
  [_style-fn _value]
  {})

;; ---------------------------------------------------------------------------
;; spec table editor
;; ---------------------------------------------------------------------------

(defn- method-names
  "Keyword dispatch values of a multimethod, except :default, sorted by name."
  [multifn]
  (->> (keys (methods multifn))
       (remove #{:default})
       (sort-by name)))

(defn- data-columns
  "Distinct map keys across DATA, in first-seen order."
  [data]
  (reduce (fn [ks row]
            (if (map? row)
              (reduce (fn [ks k]
                        (if (some #(= k %) ks) ks (conj ks k)))
                      ks (keys row))
              ks))
          [] data))

(defn- spec-paths
  "Non-nil :path values from the spec."
  [spec]
  (keep :path spec))

(defn- selectable-paths
  "Data columns plus spec paths that are absent from the data."
  [data spec]
  (let [cols (data-columns data)]
    (vec (concat cols
                 (->> (spec-paths spec)
                      (remove #(some (fn [c] (= c %)) cols))
                      distinct)))))

(defn- path-option-value
  "String form of a path (keywords stay EDN-readable)."
  [path]
  (pr-str path))

(defn- path-label
  [path]
  (cond
    (keyword? path) (name path)
    (nil? path)     "\u2014"
    :else           (str path)))

(defn- column-options
  "Selectable column options {:value <string> :label <string>}, with a leading
   blank option for rows without a :path."
  [data spec]
  (cons {:value "" :label "\u2014"}
        (map (fn [path] {:value (path-option-value path)
                         :label (path-label path)})
             (selectable-paths data spec))))

(defn- read-spec
  "Current spec: parsed from code-a when it is a non-blank EDN vector, else the
   :spec argument, else []."
  [opts]
  (let [c (:code-a opts)
        text (try (when c @c) (catch Exception _ nil))
        parsed (try
                 (when (and (string? text) (not (str/blank? text)))
                   (edn/read-string text))
                 (catch Exception _ nil))]
    (cond
      (vector? parsed)   parsed
      (vector? (:spec opts)) (:spec opts)
      (sequential? (:spec opts)) (vec (:spec opts))
      :else [])))

(defn- write-spec!
  [code-a spec]
  (reset! code-a (pr-str (vec spec))))

(defn- update-row!
  [opts idx f]
  (write-spec! (:code-a opts)
               (mapv (fn [i row] (if (= i idx) (f row) row))
                     (range) (read-spec opts))))

(defn- assoc-row!
  [opts idx k v]
  (update-row! opts idx (fn [row] (assoc row k v))))

(defn- dissoc-row!
  [opts idx k]
  (update-row! opts idx (fn [row] (dissoc row k))))

(defn- delete-row!
  [opts idx]
  (write-spec! (:code-a opts)
               (keep-indexed (fn [i row] (when (not= i idx) row))
                             (read-spec opts))))

(defn- add-row!
  [opts row]
  (write-spec! (:code-a opts) (conj (read-spec opts) row)))

(defn- parse-width
  "Whole numbers become longs, other finite numbers doubles; nil when the text
   is blank or not numeric."
  [s]
  (when-not (str/blank? s)
    (try
      (let [d (parse-double s)]
        (when (and d (not (Double/isNaN d)) (not (Double/isInfinite d)))
          (if (== d (Math/rint d)) (long d) d)))
      (catch Exception _ nil))))

(def ^:private table-style
  {:border-collapse "collapse" :table-layout "fixed"
   :font-size "0.8rem" :font-family "system-ui, sans-serif"
   :width "100%"})

(def ^:private th-style
  {:text-align "left" :padding "0.3rem 0.4rem" :border-bottom "2px solid #e5e7eb"
   :background "#f9fafb" :color "#374151" :font-weight "600"
   :font-size "0.7rem" :text-transform "uppercase" :letter-spacing "0.03em"
   :box-sizing "border-box"})

(def ^:private td-style
  {:padding "0.3rem 0.4rem" :border-bottom "1px solid #eef2f7"
   :vertical-align "middle" :box-sizing "border-box"})

(def ^:private input-style
  {:width "100%" :box-sizing "border-box" :padding "0.25rem 0.4rem"
   :border "1px solid #d1d5db" :border-radius "0.375rem" :font-size "0.8rem"})

(def ^:private select-style
  {:width "100%" :min-width 0 :box-sizing "border-box" :padding "0.25rem 0.3rem"
   :border "1px solid #d1d5db" :border-radius "0.375rem" :font-size "0.8rem"
   :background "white"})

(def ^:private delete-col-width "1.6rem")
(def ^:private column-col-width "8rem")
(def ^:private width-col-width  "4.5rem")
(def ^:private format-col-width  "7rem")
(def ^:private opts-col-width    "6rem")
(def ^:private style-col-width   "7rem")

(def ^:private delete-btn-style
  {:border "1px solid #fca5a5"
   :background "#fef2f2"
   :color "#b91c1c"
   :cursor "pointer"
   :border-radius "0.375rem"
   :font-size "0.9rem"
   :line-height "1"
   :padding "0.2rem 0.45rem"})

(def ^:private add-btn-style
  {:border "1px solid #2563eb" :background "#2563eb" :color "white"
   :cursor "pointer" :border-radius "0.375rem" :font-weight "700"
   :font-size "1rem" :line-height "1" :padding "0.3rem 0.6rem"})

(defn rtable-format-edit-ui
  "Edit an rtable column spec as a table.

   data  the table rows (used to list selectable columns)
   opts  {:spec <current spec> :code-a <hyper cursor holding EDN text>}

   One row per spec entry: delete, column, width, format, format-opts, style.
   The footer + button appends an entry seeded with the first data column.
   Every edit resets code-a to (pr-str spec); the spec popup persists it on
   Close.  Must be called inside a Hyper render context."
  [data opts]
  (let [spec     (read-spec opts)
        col-opts (column-options data spec)
        formats  (method-names format-rtable-col)
        styles   (method-names style-rtable-col)
        first-col (first (data-columns data))]
    [:div {:style {:overflow "auto" :max-height "60vh"}}
     [:table {:style table-style}
      [:thead
       [:tr
        [:th {:style (assoc th-style :width delete-col-width)} ""]
        [:th {:style (assoc th-style :width column-col-width)} "Column"]
        [:th {:style (assoc th-style :width width-col-width)} "Width"]
        [:th {:style (assoc th-style :width format-col-width)} "Format"]
        [:th {:style (assoc th-style :width opts-col-width)} "Options"]
        [:th {:style (assoc th-style :width style-col-width)} "Style"]]]
      [:tbody
       (for [[idx row] (map-indexed vector spec)]
         [:tr {:key idx}
          [:td {:style (assoc td-style :text-align "center")}
           [:button {:type "button" :title "Delete" :aria-label "Delete"
                     :style delete-btn-style
                     :data-on:click (h/action (delete-row! opts idx))}
            "\u00d7"]]

          [:td {:style td-style}
           [:select {:style select-style
                     :data-on:change
                     (h/action
                      (if (str/blank? $value)
                        (dissoc-row! opts idx :path)
                        (assoc-row! opts idx :path
                                    (edn/read-string $value))))}
            (for [opt col-opts]
              [:option {:value (:value opt)
                        :selected (= (:value opt)
                                     (some-> (:path row) path-option-value))}
               (:label opt)])]]

          [:td {:style td-style}
           [:input {:type "number" :min "0" :step "1"
                    :value (if (some? (:width row)) (str (:width row)) "")
                    :style input-style
                    :data-on:change
                    (h/action
                     (if-let [w (parse-width $value)]
                       (assoc-row! opts idx :width w)
                       (dissoc-row! opts idx :width)))}]]

          [:td {:style td-style}
           [:select {:style select-style
                     :data-on:change
                     (h/action
                      (if (str/blank? $value)
                        (dissoc-row! opts idx :format)
                        (assoc-row! opts idx :format (keyword $value))))}
            [:option {:value "" :selected (not (keyword? (:format row)))} "\u2014"]
            (for [fmt formats]
              [:option {:value (name fmt)
                        :selected (= fmt (:format row))}
               (name fmt)])]]

          [:td {:style td-style}
           [:input {:type "text"
                    :value (or (some-> (:format-opts row) str) "")
                    :style input-style
                    :data-on:change
                    (h/action
                     (if (str/blank? $value)
                       (dissoc-row! opts idx :format-opts)
                       (assoc-row! opts idx :format-opts $value)))}]]

          [:td {:style td-style}
           [:select {:style select-style
                     :data-on:change
                     (h/action
                      (if (str/blank? $value)
                        (dissoc-row! opts idx :style-fn)
                        (assoc-row! opts idx :style-fn (keyword $value))))}
            [:option {:value "" :selected (not (keyword? (:style-fn row)))} "\u2014"]
            (for [s styles]
              [:option {:value (name s)
                        :selected (= s (:style-fn row))}
               (name s)])]]])]
      [:tfoot
       [:tr
        [:td {:style (assoc td-style :text-align "center")}
         [:button {:type "button" :title "Add column" :aria-label "Add column"
                   :style add-btn-style
                   :data-on:click
                   (h/action (add-row! opts (cond-> {} first-col (assoc :path first-col))))}
          "+"]]
        [:td {:style td-style :colspan 5}]]]]]))