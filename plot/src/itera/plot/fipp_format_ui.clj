(ns itera.plot.fipp-format-ui
  "Spec editor for itera.plot.fipp viewers.

   Edits the map spec {:width <positive-int>
                       :print-length <non-negative-int-or-absent>
                       :print-level <non-negative-int-or-absent>
                       :print-meta <boolean>} which is passed to
   fipp.edn/pprint.  Blank numeric inputs remove the key (width then falls
   back to fipp's default width, counts mean unlimited), and invalid numeric
   input leaves the spec unchanged.  Keys outside the four supported options
   are preserved while editing.

   Every edit resets the caller's code cursor to (pr-str spec); the existing
   spec popup persists it when it closes.  Must be called inside a Hyper
   render context."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hyper.core :as h]))

(def default-options
  "Options used when a spec is missing; identical to fipp.edn/pprint's own
   defaults.  The nil counts mean unlimited."
  {:width        70
   :print-length nil
   :print-level  nil
   :print-meta   false})

(def option-keys
  "The fipp.edn/pprint options the viewer and editor support."
  [:width :print-length :print-level :print-meta])

(defn- valid-width?
  [v]
  (and (integer? v) (pos? v)))

(defn- valid-count?
  [v]
  (and (integer? v) (not (neg? v))))

(defn- valid-meta?
  [v]
  (boolean? v))

(defn normalize-options
  "Sanitize SPEC into an options map for fipp.edn/pprint.

   nil returns default-options, a non-map returns nil (invalid), and a map is
   reduced to the supported keys whose values have the right type; unknown
   keys and invalid values are dropped.  Absent :print-length / :print-level
   mean unlimited."
  [spec]
  (cond
    (nil? spec)       default-options
    (not (map? spec)) nil
    :else
    (let [{:keys [width print-length print-level print-meta]} spec]
      (cond-> {}
        (valid-width? width)   (assoc :width width)
        (valid-count? print-length) (assoc :print-length print-length)
        (valid-count? print-level)  (assoc :print-level print-level)
        (valid-meta? print-meta)    (assoc :print-meta print-meta)))))

(defn- read-spec
  "Current spec map: parsed from code-a when it holds a valid EDN map, else
   SPEC when it is a map, else default-options.  Unknown keys are preserved."
  [spec code-a]
  (let [text   (try (when code-a @code-a) (catch Exception _ nil))
        parsed (try
                 (when (and (string? text) (not (str/blank? text)))
                   (edn/read-string text))
                 (catch Exception _ nil))]
    (cond
      (map? parsed) parsed
      (map? spec)   spec
      :else         default-options)))

(defn- write-spec!
  [code-a spec]
  (reset! code-a (pr-str spec)))

(defn- parse-integer
  "Whole number from text; nil when the text is blank, not numeric or not a
   finite integer."
  [s]
  (when-not (str/blank? s)
    (try
      (let [d (parse-double s)]
        (when (and d
                   (not (Double/isNaN d))
                   (not (Double/isInfinite d))
                   (== d (Math/rint d)))
          (long d)))
      (catch Exception _ nil))))

(defn- set-width!
  [spec code-a value]
  (if (str/blank? value)
    (write-spec! code-a (dissoc (read-spec spec code-a) :width))
    (when-let [n (parse-integer value)]
      (when (pos? n)
        (write-spec! code-a (assoc (read-spec spec code-a) :width n))))))

(defn- set-count!
  [spec code-a k value]
  (if (str/blank? value)
    (write-spec! code-a (dissoc (read-spec spec code-a) k))
    (when-let [n (parse-integer value)]
      (when (not (neg? n))
        (write-spec! code-a (assoc (read-spec spec code-a) k n))))))

(defn- set-print-meta!
  [spec code-a checked]
  (write-spec! code-a (assoc (read-spec spec code-a) :print-meta (boolean checked))))

(def ^:private row-style
  {:display "flex" :align-items "center" :gap "0.5rem"})

(def ^:private label-style
  {:width "7rem" :flex "0 0 7rem" :color "#374151"})

(def ^:private input-style
  {:box-sizing "border-box" :padding "0.25rem 0.4rem"
   :border "1px solid #d1d5db" :border-radius "0.375rem"
   :font-size "0.8rem"})

(def ^:private input-row-style
  (assoc input-style :flex "1" :min-width "0"))

(defn fipp-format-edit-ui
  "Edit a fipp spec bound to code-a.

   Four controls set :width, :print-length, :print-level and :print-meta.
   Blank numbers remove the key; invalid numbers leave the spec unchanged.
   Must be called inside a Hyper render context."
  [spec code-a]
  (let [current (read-spec spec code-a)]
    [:div {:style {:display "flex" :flex-direction "column" :gap "0.5rem"
                   :font-family "system-ui, sans-serif" :font-size "0.8rem"}}
     [:label {:style row-style}
      [:span {:style label-style} "Width"]
      [:input {:type "number" :min "1" :step "1"
               :aria-label "Line width"
               :title "Maximum line width in characters"
               :placeholder (str (:width default-options))
               :value (if (some? (:width current)) (str (:width current)) "")
               :style input-row-style
               :data-on:change (h/action (set-width! spec code-a $value))}]]

     [:label {:style row-style}
      [:span {:style label-style} "Print length"]
      [:input {:type "number" :min "0" :step "1"
               :aria-label "Print length"
               :title "Maximum items per collection; blank means unlimited"
               :placeholder "unlimited"
               :value (if (some? (:print-length current))
                        (str (:print-length current)) "")
               :style input-row-style
               :data-on:change
               (h/action (set-count! spec code-a :print-length $value))}]]

     [:label {:style row-style}
      [:span {:style label-style} "Print level"]
      [:input {:type "number" :min "0" :step "1"
               :aria-label "Print level"
               :title "Maximum nesting depth; blank means unlimited"
               :placeholder "unlimited"
               :value (if (some? (:print-level current))
                        (str (:print-level current)) "")
               :style input-row-style
               :data-on:change
               (h/action (set-count! spec code-a :print-level $value))}]]

     [:label {:style row-style}
      [:span {:style label-style} "Print meta"]
      [:input {:type "checkbox"
               :aria-label "Print meta"
               :title "Print metadata on values that have it"
               :checked (boolean (:print-meta current))
               :style {:margin "0"}
               :data-on:change
               (h/action (set-print-meta! spec code-a $checked))}]]

     [:div {:style {:color "#6b7280" :font-size "0.7rem"}}
      "Blank width uses fipp's default (70); blank counts mean unlimited."]]))