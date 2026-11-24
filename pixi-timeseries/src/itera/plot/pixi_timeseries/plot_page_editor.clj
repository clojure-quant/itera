(ns itera.plot.pixi-timeseries.plot-page-editor
  "Row-window and page editor for the pixi-timeseries viewer.

   The selector value is
   {:display :nice|:all
    :step <pixels per row>
    :end <exclusive end row>
    :rows-all <total result rows>
    :rows-visible <rows per page>
    :aux-pane <first visible aux pane index>}.

   `selector-ui` renders the display/step comboboxes plus the range slider and
   the first/prior/next/last buttons over that value.  :display defaults to
   :nice (windowed: slider + nav buttons + a \"Showing start–end of N\"
   summary); with :all the slider and nav buttons are hidden and an \"All N
   rows\" summary is shown instead.  :step has no effect
   on row selection in :all, but in :nice it drives the derived
   rows-per-viewport.

   When the optional :pane-spec (the charts spec map) and :window are
   supplied, the pane layout is computed with
   itera.plot.pixi-timeseries.pane-layout and, when some aux panes do not fit,
   a compact \"Aux pane\" combobox is rendered next to the row slider (in
   :nice) or the Display/Step combos (in :all).  Choosing an entry writes the
   clamped first-visible-aux-pane index into :aux-pane.  Without :pane-spec
   (the legacy adapter) no aux control is rendered.

   In :nice mode the rows per viewport are derived from the live `:window`
   :width and :step on every render (so a container resize re-windows the
   slice and updates the slider bounds, nav step, and summary); the passed
   `rows-visible` is only a fallback for when no `:window` is supplied or its
   :width is missing/invalid.

   `page-editor-view` binds `selector-ui` to a page cursor for the paged
   itera.value.protocol implementation; the page map supplies :rows-all and
   :rows-visible.  With no page cursor the controls render without actions.

   The legacy :window-selector option editor is kept as a thin adapter over
   `selector-ui` so the demo algo page keeps working; it passes no `:window`
   or `:pane-spec` and relies on `init-window-selector!` having refreshed
   :rows-visible, so :rows-all/:rows-visible may also come from the option
   :spec there.

   This namespace is loaded via :option-ui in a project's itera-plots.edn by
   `itera.hyper/start`; it deliberately does not depend on the plot namespace."
  (:require [hyper.core :as h]
            [itera.option.core :as options]
            [itera.plot.pixi-timeseries.pane-layout :as pane-layout]
            [itera.plot.pixi-timeseries.window :as plot-window]
            [itera.value.protocol :as spec-plot]))

(def ^:private field-style
  {:width "100%"})

(def ^:private label-style
  {:display "flex" :flex-direction "column"
   :font-size "0.68rem" :color "#374151" :gap "0.1rem"})

(def ^:private button-style
  {:font-size "0.68rem" :padding "0.15rem 0.4rem" :cursor "pointer"
   :border "1px solid #d1d5db" :border-radius "0.2rem" :background "#f3f4f6"})

(def ^:private summary-style
  {:font-size "0.68rem" :color "#6b7280"})

(defn- clamp [lo hi v]
  (max lo (min hi v)))

(defn- ->int
  "Parse an input value; nil for blank/invalid input."
  [v]
  (parse-long (str v)))

(defn- display-select
  "The :display combobox.  Writes a keyword :display."
  [value update* path display-options actions?]
  (let [display (or (:display value) :nice)]
    [:label {:style label-style} "Display"
     (into [:select (cond-> {:style field-style}
                      (not actions?) (assoc :disabled true)
                      actions? (assoc :data-on:change
                                      (h/action
                                       (update* path
                                                #(assoc (or % {}) :display
                                                        (keyword $value))))))]
           (for [d display-options]
             [:option {:value (name d) :selected (= d display)} (name d)]))]))

(defn- step-select
  "The :step combobox (pixels per row).  Writes an integer :step, ignoring
   blank/invalid input."
  [value update* path step-options actions?]
  (let [sraw (let [s (:step value)]
               (if (and (number? s) (pos? s)) s 8))]
    [:label {:style label-style} "Step"
     (into [:select (cond-> {:style field-style}
                      (not actions?) (assoc :disabled true)
                      actions? (assoc :data-on:change
                                      (h/action
                                       (update* path
                                                (fn [sel]
                                                  (let [n (parse-long $value)]
                                                    (if n
                                                      (assoc (or sel {}) :step n)
                                                      (or sel {}))))))))]
           (for [s step-options]
             [:option {:value (str s) :selected (= s sraw)} (str s)]))]))

(defn- aux-select
  "Compact \"Aux pane\" combobox selecting the first visible aux pane.

   Rendered only when PLAN's :overflow? is true (some aux panes are hidden).
   Options are 0..:max-start labelled \"Aux 1\", \"Aux 2\", ...; a change writes
   the clamped integer :aux-pane."
  [value update* path plan actions?]
  (when (:overflow? plan)
    (let [max-start (:max-start plan)
          craw      (:aux-pane value)
          cur       (clamp 0 max-start (if (number? craw) (long craw) 0))]
      [:label {:style label-style} "Aux pane"
       (into [:select (cond-> {:style field-style}
                        (not actions?) (assoc :disabled true)
                        actions? (assoc :data-on:change
                                        (h/action
                                         (update* path
                                                  (fn [sel]
                                                    (let [n (or (->int $value) cur)]
                                                      (assoc (or sel {}) :aux-pane
                                                             (clamp 0 max-start n))))))))]
             (for [n (range (inc max-start))]
               [:option {:value (str n) :selected (= n cur)}
                (str "Aux " (inc n))]))])))

(defn selector-ui
  "Render the row-window controls.

   value            the selector map ({:display :step :end :rows-all
                    :rows-visible})
   update*          two-arg (fn [path f]) read-modify-write of the value
   path             path passed through to update*
   class / style    container passthrough
   display-options  :display choices (default [:nice :all])
   step-options     :step choices (default [4 6 8 10 12 14])
   rows-all         total row count; nil renders a missing-row-count message
   rows-visible     rows per viewport fallback (default 1)
   window           window map; when supplied with a positive :width, the rows
                    per viewport are derived from it and :step (overriding
                    rows-visible)
   pane-spec        optional charts spec map; with :window it computes the pane
                    layout and shows the \"Aux pane\" combobox when some aux
                    panes do not fit
   actions?         when false the controls render disabled with no actions"
  [{:keys [value update* path class style display-options step-options
           rows-all rows-visible window pane-spec actions?]
    :or {display-options [:nice :all]
         step-options [4 6 8 10 12 14]
         actions? true}}]
  (let [display (or (:display value) :nice)
        plan    (when pane-spec
                  (pane-layout/layout pane-spec window (:aux-pane value)))
        aux?    (boolean (:overflow? plan))
        combos  (into [:div {:style {:display "flex" :gap "0.35rem"
                                     :align-items "flex-end"}}]
                      (keep identity
                            [(display-select value update* path display-options actions?)
                             (step-select value update* path step-options actions?)
                             (when aux? (aux-select value update* path plan actions?))]))
        shell   {:class class
                 :style (merge {:display "flex" :flex-direction "column"
                                :gap "0.35rem"} style)}]
    (cond
      (not (and (number? rows-all) (not (neg? rows-all))))
      [:div {:class class
             :style (merge {:color "#b91c1c" :font-size "0.75rem"} style)}
       "Window Selector: row count not available"]

      (zero? rows-all)
      [:div {:class class :style (merge summary-style style)}
       "0 rows"]

      (= display :all)
      [:div shell
       combos
       [:div {:style summary-style} (str "All " rows-all " rows")]]

      :else
      (let [derived (when window (plot-window/rows-visible window value))
            vraw    (or rows-visible 1)
            visible (clamp 1 rows-all
                           (long (or derived (if (number? vraw) vraw 1))))
            eraw    (:end value)
            end     (clamp visible rows-all
                           (long (if (number? eraw) eraw rows-all)))
            start   (inc (max 0 (- end visible)))
            button  (fn [label f]
                      [:button (cond-> {:type "button" :style button-style}
                                 (not actions?) (assoc :disabled true)
                                 actions? (assoc :data-on:click
                                                 (h/action (update* path f))))
                       label])
            slider  (fn [style]
                      [:input (cond-> {:type "range"
                                       :min (str visible) :max (str rows-all)
                                       :step 1
                                       :value (str end)
                                       :style style}
                                (not actions?) (assoc :disabled true)
                                actions? (assoc :data-on:change
                                                (h/action
                                                 (update* path
                                                          (fn [sel]
                                                            (let [n (or (->int $value) end)]
                                                              (assoc (or sel {})
                                                                     :end (clamp visible
                                                                                 rows-all n))))))))])
            aux        (when aux? (aux-select value update* path plan actions?))
            slider-row (if aux
                         [:div {:style {:display "flex" :gap "0.35rem"
                                        :align-items "flex-end"}}
                          (slider (assoc field-style :flex "1" :min-width "0"
                                         :width "auto"))
                          aux]
                         (slider field-style))]
        [:div shell
         combos
         slider-row
         [:div {:style {:display "flex" :gap "0.25rem"}}
          (button "<|" (fn [sel] (assoc (or sel {}) :end visible)))
          (button "<" (fn [sel]
                        (let [cur (or (:end sel) rows-all)]
                          (assoc (or sel {})
                                 :end (clamp visible rows-all
                                             (- cur visible))))))
          (button ">" (fn [sel]
                        (let [cur (or (:end sel) rows-all)]
                          (assoc (or sel {})
                                 :end (clamp visible rows-all
                                             (+ cur visible))))))
          (button ">|" (fn [sel] (assoc (or sel {}) :end rows-all)))]
         [:div {:style summary-style}
          (str "Showing " start "–" end " of " rows-all)]]))))

(defmethod options/editor :window-selector [opt value set-fn update-fn _group]
  (let [path    (options/norm-path (:path opt))
        update* (or update-fn (fn [p f] (set-fn p (f value))))]
    (selector-ui
     {:value value
      :update* update*
      :path path
      :class (:class opt)
      :style (:style opt)
      :display-options (get-in opt [:spec :display] [:nice :all])
      :step-options (get-in opt [:spec :step] [4 6 8 10 12 14])
      :rows-all (or (:rows-all value) (get-in opt [:spec :rows-all]))
      :rows-visible (or (:rows-visible value) (get-in opt [:spec :rows-visible]))
      :actions? true})))

(defn page-editor-view
  "Render the row-window controls for plot value V over the page cursor PAGE-A.

   PAGE-A derefs to the page map {:display :step :end :rows-all :rows-visible}
   and is mutated by every control.  When PAGE-A is nil the page map comes from
   (init-page v spec window) and the controls render without actions."
  [v spec window page-a]
  (let [page    (if page-a
                  @page-a
                  (if (satisfies? spec-plot/itera-page-plot v)
                    (spec-plot/init-page v spec window)
                    {:display :nice :step 8 :rows-all 0}))
        update* (when page-a (fn [_path f] (swap! page-a f)))]
    (selector-ui
     {:value page
      :update* update*
      :path [:window-selector]
      :window window
      :pane-spec spec
      :rows-all (:rows-all page)
      :rows-visible (:rows-visible page)
      :actions? (some? page-a)})))