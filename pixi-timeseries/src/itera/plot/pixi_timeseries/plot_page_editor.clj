(ns itera.plot.pixi-timeseries.plot-page-editor
  "Row-window and page editor for the pixi-timeseries viewer.

   The selector value is
   {:display :nice|:all
    :step <pixels per row>
    :end <exclusive end row>
    :rows-all <total result rows>
    :rows-visible <rows per page>}.

   `selector-ui` renders the display/step comboboxes plus the range slider and
   the first/prior/next/last buttons over that value.  :display defaults to
   :nice (windowed: slider + nav buttons + a \"Showing start–end of N\"
   summary); with :all the slider and nav buttons are hidden and an \"All N
   rows\" summary is shown instead.  :step is still stored but has no effect on
   row selection.

   `page-editor-view` binds `selector-ui` to a page cursor for the paged
   itera.value.protocol implementation; the page map supplies :rows-all and
   :rows-visible.  With no page cursor the controls render without actions.

   The legacy :window-selector option editor is kept as a thin adapter over
   `selector-ui` so the demo algo page keeps working; :rows-all/:rows-visible
   may also come from the option :spec there.

   This namespace is loaded via :option-ui in a project's itera-plots.edn by
   `itera.hyper/start`; it deliberately does not depend on the plot namespace."
  (:require [hyper.core :as h]
            [itera.option.core :as options]
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
   actions?         when false the controls render disabled with no actions"
  [{:keys [value update* path class style display-options step-options
           rows-all rows-visible actions?]
    :or {display-options [:nice :all]
         step-options [4 6 8 10 12 14]
         actions? true}}]
  (let [display (or (:display value) :nice)
        combos  [:div {:style {:display "flex" :gap "0.35rem"}}
                 (display-select value update* path display-options actions?)
                 (step-select value update* path step-options actions?)]
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
      (let [vraw    (or rows-visible 1)
            visible (clamp 1 rows-all
                           (long (if (number? vraw) vraw 1)))
            eraw    (:end value)
            end     (clamp visible rows-all
                           (long (if (number? eraw) eraw rows-all)))
            start   (inc (max 0 (- end visible)))
            button  (fn [label f]
                      [:button (cond-> {:type "button" :style button-style}
                                 (not actions?) (assoc :disabled true)
                                 actions? (assoc :data-on:click
                                                 (h/action (update* path f))))
                       label])]
        [:div shell
         combos
         [:input (cond-> {:type "range"
                          :min (str visible) :max (str rows-all)
                          :step 1
                          :value (str end)
                          :style field-style}
                   (not actions?) (assoc :disabled true)
                   actions? (assoc :data-on:change
                                   (h/action
                                    (update* path
                                             (fn [sel]
                                               (let [n (or (->int $value) end)]
                                                 (assoc (or sel {})
                                                        :end (clamp visible
                                                                    rows-all n))))))))]
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
      :rows-all (:rows-all page)
      :rows-visible (:rows-visible page)
      :actions? (some? page-a)})))