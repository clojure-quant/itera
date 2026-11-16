(ns itera.plot.pixi-timeseries.options.row-select
  "The :window-selector option editor for the pixi-timeseries viewer.

   Renders the display/step comboboxes plus the range slider and the
   first/prior/next/last buttons that previously lived in the browser
   component.  The selector value is
   {:display :nice|:all
    :step <pixels per row>
    :end <exclusive end row>
    :rows-all <total result rows>
    :rows-visible <rows per page>};
   this editor writes :display, :step and :end and preserves the other keys on
   every update.

   :display defaults to :nice (windowed: slider + nav buttons + a
   \"Showing start–end of N\" summary).  With :all the slider and nav buttons
   are hidden and a \"All N rows\" summary is shown instead; :step is still
   stored but has no effect on row selection.

   :rows-all and :rows-visible normally come from
   `itera.plot.pixi-timeseries.plot/init-window-selector!`, installed as the
   algo's :plot/select hook (the demo also seeds :rows-all in its initial plot
   options).  The option :spec may supply fallbacks for direct callers; without
   :rows-all there are no valid slider bounds.

   This namespace is loaded via :option-ui in a project's itera-plots.edn by
   `itera.hyper/start`; it deliberately does not depend on the plot namespace."
  (:require [hyper.core :as h]
            [itera.options :as options]))

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
  "The :display combobox; options come from the option :spec :display and
   default to [:nice :all].  Writes a keyword :display."
  [opt value update* path]
  (let [display (or (:display value) :nice)
        opts    (get-in opt [:spec :display] [:nice :all])]
    [:label {:style label-style} "Display"
     (into [:select {:style field-style
                     :data-on:change
                     (h/action
                      (update* path
                               #(assoc (or % {}) :display (keyword $value))))}]
           (for [d opts]
             [:option {:value (name d) :selected (= d display)} (name d)]))]))

(defn- step-select
  "The :step combobox (pixels per row); options come from the option :spec
   :step and default to [4 6 8 10 12 14].  Writes an integer :step, ignoring
   blank/invalid input."
  [opt value update* path]
  (let [sraw (let [s (:step value)]
               (if (and (number? s) (pos? s)) s 8))
        opts (get-in opt [:spec :step] [4 6 8 10 12 14])]
    [:label {:style label-style} "Step"
     (into [:select {:style field-style
                     :data-on:change
                     (h/action
                      (update* path
                               (fn [sel]
                                 (let [n (parse-long $value)]
                                   (if n (assoc (or sel {}) :step n) (or sel {}))))))}]
           (for [s opts]
             [:option {:value (str s) :selected (= s sraw)} (str s)]))]))

(defmethod options/editor :window-selector [opt value set-fn update-fn _group]
  (let [path     (options/norm-path (:path opt))
        update*  (or update-fn (fn [p f] (set-fn p (f value))))
        rows-all (or (:rows-all value) (get-in opt [:spec :rows-all]))]
    (cond
      (not (and (number? rows-all) (not (neg? rows-all))))
      [:div {:class (:class opt)
             :style (merge {:color "#b91c1c" :font-size "0.75rem"} (:style opt))}
       "Window Selector: row count not available"]

      (zero? rows-all)
      [:div {:class (:class opt) :style (merge summary-style (:style opt))}
       "0 rows"]

      :else
      (let [display (or (:display value) :nice)
            combos  [:div {:style {:display "flex" :gap "0.35rem"}}
                     (display-select opt value update* path)
                     (step-select opt value update* path)]
            shell   {:class (:class opt)
                     :style (merge {:display "flex" :flex-direction "column"
                                    :gap "0.35rem"}
                                   (:style opt))}]
        (if (= display :all)
          [:div shell
           combos
           [:div {:style summary-style} (str "All " rows-all " rows")]]
          (let [vraw    (or (:rows-visible value)
                            (get-in opt [:spec :rows-visible])
                            1)
                visible (clamp 1 rows-all
                               (long (if (number? vraw) vraw 1)))
                eraw    (:end value)
                end     (clamp visible rows-all
                               (long (if (number? eraw) eraw rows-all)))
                start   (inc (max 0 (- end visible)))]
            [:div shell
             combos
             [:input {:type "range"
                      :min (str visible) :max (str rows-all)
                      :step 1
                      :value (str end)
                      :style field-style
                      :data-on:change
                      (h/action
                       (update* path
                                (fn [sel]
                                  (let [n (or (->int $value) end)]
                                    (assoc (or sel {})
                                           :end (clamp visible rows-all n))))))}]
             [:div {:style {:display "flex" :gap "0.25rem"}}
              [:button {:type "button" :style button-style
                        :data-on:click
                        (h/action
                         (update* path
                                  (fn [sel]
                                    (assoc (or sel {}) :end visible))))}
               "<|"]
              [:button {:type "button" :style button-style
                        :data-on:click
                        (h/action
                         (update* path
                                  (fn [sel]
                                    (let [cur (or (:end sel) rows-all)]
                                      (assoc (or sel {})
                                             :end (clamp visible rows-all
                                                         (- cur visible)))))))}
               "<"]
              [:button {:type "button" :style button-style
                        :data-on:click
                        (h/action
                         (update* path
                                  (fn [sel]
                                    (let [cur (or (:end sel) rows-all)]
                                      (assoc (or sel {})
                                             :end (clamp visible rows-all
                                                         (+ cur visible)))))))}
               ">"]
              [:button {:type "button" :style button-style
                        :data-on:click
                        (h/action
                         (update* path
                                  (fn [sel]
                                    (assoc (or sel {}) :end rows-all))))}
               ">|"]]
             [:div {:style summary-style}
              (str "Showing " start "–" end " of " rows-all)]]))))))
