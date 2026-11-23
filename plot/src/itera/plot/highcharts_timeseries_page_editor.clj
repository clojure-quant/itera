(ns itera.plot.highcharts-timeseries-page-editor
  "Row-window editor for the highcharts-timeseries viewer.

   The page map is
   {:step <pixels per row>
    :end <exclusive end row>
    :rows-all <total rows>
    :rows-visible <rows per window>}.

   `page-editor-view` renders a \"Pixels per row\" select plus a full-width
   range slider and a \"Showing start–end of N\" summary over that value.  The
   rows per window are derived from the live `:window` :width and :step on
   every render (so a container resize re-windows the slice and updates the
   slider bounds and summary); the passed/stored :rows-visible is only a
   fallback for when no `:window` is supplied or its :width is
   missing/invalid.

   This is a leaf namespace: it depends only on Hyper and deliberately does
   not depend on the viewer (which loads it) to avoid a require cycle.  The
   range slider covers navigation, so there is no first/prev/next/last control
   set or :display selector.

   `page-editor-view` is a pure public fn taking the page map and an optional
   page cursor; with no cursor the controls render disabled and register no
   actions (so it can be called outside a Hyper request)."
  (:require [hyper.core :as h]))

(def default-step
  "Pixels per row used when the page map has no positive :step."
  8)

(def step-options
  "Selectable pixels-per-row values."
  [4 6 8 10 12 14])

(def ^:private field-style
  {:width "100%"})

(def ^:private label-style
  {:display "flex" :flex-direction "column"
   :font-size "0.68rem" :color "#374151" :gap "0.1rem"})

(def ^:private container-style
  {:display "flex" :flex-direction "column" :gap "0.35rem"
   :font-family "system-ui, sans-serif"})

(def ^:private summary-style
  {:font-size "0.68rem" :color "#6b7280"})

(def ^:private error-style
  {:font-size "0.75rem" :color "#b91c1c"})

(defn pixels-per-row
  "Positive pixels-per-row from PAGE's :step, defaulting to `default-step` for
   a missing/invalid step."
  [page]
  (let [s (:step page)]
    (if (and (number? s) (pos? s)) s default-step)))

(defn derived-rows-visible
  "Rows per window from WINDOW's :width and PAGE's :step, as
   `(max 1 (long (Math/floor (/ width step))))`.

   nil when WINDOW's :width is not a positive number (e.g. an unmeasured
   :container window); callers fall back to a stored row count in that case.
   Does not clamp to a row count."
  [window page]
  (let [w (:width window)]
    (when (and (number? w) (pos? w))
      (max 1 (long (Math/floor (/ (double w) (pixels-per-row page))))))))

(defn visible-rows
  "Rows per window for N rows, clamped to [1 N] (0 for N = 0): the
   width-derived count when available, else PAGE's stored :rows-visible, else
   N."
  [window page n]
  (if (pos? n)
    (let [stored (:rows-visible page)]
      (max 1 (min n (long (or (derived-rows-visible window page)
                              (when (and (number? stored) (pos? stored)) stored)
                              n)))))
    0))

(defn clamp-end
  "Exclusive end row clamped to [VISIBLE N] (0 for N = 0).  A missing or
   non-numeric END defaults to N (the last window)."
  [n visible end]
  (if (pos? n)
    (max visible (min n (long (if (number? end) end n))))
    0))

(defn initial-page
  "Fresh page map for N rows at WINDOW: the default step, every row shown
   (end = N) and the width-derived :rows-visible."
  [window n]
  (let [page {:step default-step :end n :rows-all n}]
    (assoc page :rows-visible (visible-rows window page n))))

(defn page-editor-view
  "Render the row-window controls for PAGE over WINDOW and N rows.

   PAGE-A, when present, is deref'd for the current position and is mutated by
   the step select and the range slider; the controls read the fresh window
   width and N on every render.  With PAGE-A nil the controls render disabled
   with no actions."
  [page window n page-a]
  (let [actions? (some? page-a)]
    (cond
      (not (and (number? n) (not (neg? n))))
      [:div {:style (merge container-style error-style)}
       "Row count not available"]

      (zero? n)
      [:div {:style (merge container-style summary-style)}
       "0 rows"]

      :else
      (let [step    (pixels-per-row page)
            visible (visible-rows window page n)
            end     (clamp-end n visible (:end page))
            start   (inc (max 0 (- end visible)))
            update* (when actions? (fn [f] (swap! page-a f)))
            step-a  (when actions?
                      (h/action
                       (update* (fn [pg]
                                  (let [s (parse-long $value)
                                        s (if (and s (pos? s)) s default-step)
                                        pg (assoc (or pg {}) :step s)
                                        v (visible-rows window pg n)]
                                    (assoc pg :end (clamp-end n v (:end pg))))))))
            end-a   (when actions?
                      (h/action
                       (update* (fn [pg]
                                  (let [e (or (parse-long $value) end)]
                                    (assoc (or pg {}) :end
                                           (clamp-end n visible e)))))))]
        [:div {:style container-style}
         [:label {:style label-style} "Pixels per row"
          (into [:select (cond-> {:style field-style}
                           (not actions?) (assoc :disabled true)
                           actions?       (assoc :data-on:change step-a))]
                (for [s step-options]
                  [:option {:value (str s) :selected (= s step)} (str s)]))]
         [:input (cond-> {:type "range"
                          :min (str visible) :max (str n)
                          :step 1
                          :value (str end)
                          :style field-style}
                   (not actions?) (assoc :disabled true)
                   actions?       (assoc :data-on:change end-a))]
         [:div {:style summary-style}
          (str "Showing " start "\u2013" end " of " n)]]))))
