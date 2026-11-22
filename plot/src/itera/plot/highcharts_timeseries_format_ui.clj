(ns itera.plot.highcharts-timeseries-format-ui
  "Spec editor for itera.plot.highcharts-timeseries viewers.

   The editable spec stacks plots vertically:

     {:plots [{:height <px>
               :series [{:type <keyword>
                         :columns [<keyword> ...]
                         :name \"...\" :color \"blue-5\" ...} ...]} ...]}

   `highcharts-timeseries-format-edit-ui` renders nested cards: one per plot
   and one per series inside it, plus a + Plot button.  Every edit resets the
   caller's code cursor to (pr-str spec); the existing spec popup persists it
   when it closes.

   Complex attributes (:barcolor, :v2style) are edited as raw EDN text;
   blank removes the key and invalid EDN leaves the spec unchanged.  Simple
   attributes use selects / text inputs / number inputs.

   This namespace must not require itera.plot.highcharts-timeseries: the
   viewer requires this editor, not the other way around.

   Must be called inside a Hyper render context."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hyper.core :as h]
            [itera.plot.highcharts-timeseries-types :as types]
            [itera.util.color :as color]))

(def default-plot-height
  "Plot height (px) used when a plot does not set :height."
  200)

(def color-palette
  "Curated named colors offered by the series color select."
  (filterv #(contains? color/colors %)
           ["blue-5" "red-5" "green-5" "orange-5" "purple-5" "teal-5"
            "pink-5" "gray-5" "amber-5" "cyan-5" "violet-5" "lime-5"]))

(defn valid-spec?
  "True when SPEC is the map shape {:plots [<plot> ...]}."
  [spec]
  (and (map? spec)
       (vector? (:plots spec))))

(defn- default-columns
  "Column defaults for a series type whose :columns is missing/empty."
  [type]
  (case type
    :ohlc [:open :high :low :close]
    :hlc  [:high :low :close]
    :range []
    :columnrange []
    []))

(defn- series-type
  [type]
  (cond
    (keyword? type) type
    (string? type)  (keyword type)
    :else           :line))

(defn- normalize-columns
  "A :columns keyword/vector, or nil when absent."
  [series]
  (cond
    (vector? (:columns series)) (:columns series)
    (some? (:columns series))   [(:columns series)]
    (some? (:column series))    [(:column series)]
    :else                       []))

(defn- normalize-series
  "Normalize one series map: keyword :type, vector :columns, type defaults.
   Unknown keys are preserved; a legacy singular :column becomes :columns."
  [series]
  (let [series (cond
                 (map? series)     series
                 (keyword? series) {:columns [series]}
                 :else             {})
        type   (series-type (:type series))
        cols   (normalize-columns series)
        cols   (if (empty? cols) (default-columns type) cols)]
    (-> series
        (dissoc :column)
        (assoc :type type :columns (vec cols)))))

(defn- normalize-plot
  [plot]
  (let [plot   (if (map? plot) plot {})
        series (:series plot)]
    (assoc plot :series
           (mapv normalize-series (if (vector? series) series [])))))

(defn normalize-spec
  "Normalize SPEC into the editor/viewer shape, or nil when it is invalid.
   Unknown top-level, plot and series keys are preserved."
  [spec]
  (when (valid-spec? spec)
    (update spec :plots #(mapv normalize-plot %))))

(defn- read-spec
  "Current spec: parsed from code-a when it is valid, else (:spec opts) when it
   is valid, else {:plots []}."
  [opts]
  (let [c      (:code-a opts)
        text   (try (when c @c) (catch Exception _ nil))
        parsed (try
                 (when (and (string? text) (not (str/blank? text)))
                   (edn/read-string text))
                 (catch Exception _ nil))]
    (cond
      (valid-spec? parsed)      (normalize-spec parsed)
      (valid-spec? (:spec opts)) (normalize-spec (:spec opts))
      :else                     {:plots []})))

(defn- write-spec!
  [code-a spec]
  (reset! code-a (pr-str spec)))

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

(defn- spec-columns
  "Column keywords already referenced by SPEC."
  [spec]
  (for [plot  (:plots spec)
        series (:series plot)
        col    (:columns series)
        :when  (keyword? col)]
    col))

(defn- selectable-columns
  "Data columns except time columns, plus columns already used by the spec."
  [data spec]
  (let [cols (remove #{:epoch :date} (data-columns data))]
    (vec (distinct (concat cols (spec-columns spec))))))

(defn- series-id*
  "Series id used by the viewer for ids and flags targets."
  [series]
  (or (:id series)
      (:name series)
      (some-> (:columns series) first str)
      (some-> (:type series) name)))

(defn- sibling-ids
  [spec i j]
  (->> (get-in spec [:plots i :series])
       (keep-indexed (fn [k series]
                       (when (not= k j)
                         (series-id* series))))
       distinct
       vec))

(defn- default-series
  [cols]
  (let [col (first cols)]
    (cond-> {:type :line :columns (if col [col] [])}
      col (assoc :color (first color-palette)))))

(defn- resize-columns
  "Keep the existing columns up to COUNT, then pad from the type defaults or
   repeat the last available column."
  [cols type]
  (let [n        (get types/column-count type 1)
        cols     (vec (or cols []))
        defaults (default-columns type)
        fallback (or (peek cols) (first defaults) :close)]
    (vec (map (fn [i] (or (get cols i) (get defaults i) fallback))
              (range n)))))

(defn- update-plot!
  [opts i f]
  (write-spec! (:code-a opts) (update-in (read-spec opts) [:plots i] f)))

(defn- update-series!
  [opts i j f]
  (write-spec! (:code-a opts) (update-in (read-spec opts) [:plots i :series j] f)))

(defn- add-plot!
  [opts cols]
  (write-spec! (:code-a opts)
               (update (read-spec opts) :plots conj
                       {:height default-plot-height
                        :series [(default-series cols)]})))

(defn- delete-plot!
  [opts i]
  (write-spec! (:code-a opts)
               (update (read-spec opts) :plots
                       (fn [plots]
                         (vec (keep-indexed (fn [k plot]
                                              (when (not= k i) plot))
                                            plots))))))

(defn- add-series!
  [opts i cols]
  (update-plot! opts i (fn [plot]
                         (update plot :series (fnil conj [])
                                 (default-series cols)))))

(defn- delete-series!
  [opts i j]
  (update-plot! opts i
                (fn [plot]
                  (update plot :series
                          (fn [series]
                            (vec (keep-indexed (fn [k s]
                                                 (when (not= k j) s))
                                               series)))))))

(defn- set-type!
  [opts i j type]
  (update-series! opts i j
                  (fn [series]
                    (assoc series :type type
                           :columns (resize-columns (:columns series) type)))))

(defn- set-column!
  [opts i j idx value]
  (update-series! opts i j
                  (fn [series]
                    (let [cols (resize-columns (:columns series) (:type series))]
                      (assoc series :columns
                             (assoc cols idx (keyword value)))))))

(defn- parse-number
  "Whole numbers become longs, other finite numbers doubles; nil when blank or
   not numeric."
  [s]
  (when-not (str/blank? s)
    (try
      (let [d (parse-double s)]
        (when (and d
                   (not (Double/isNaN d))
                   (not (Double/isInfinite d)))
          (if (== d (Math/rint d)) (long d) d)))
      (catch Exception _ nil))))

(defn- set-number!
  [opts i j k s]
  (if (str/blank? s)
    (update-series! opts i j #(dissoc % k))
    (when-let [n (parse-number s)]
      (update-series! opts i j #(assoc % k n)))))

(defn- set-text!
  [opts i j k s]
  (if (str/blank? s)
    (update-series! opts i j #(dissoc % k))
    (update-series! opts i j #(assoc % k s))))

(defn- set-mode!
  [opts i j s]
  (update-series! opts i j #(assoc % :mode (keyword s))))

(defn- set-vec?!
  [opts i j checked]
  (update-series! opts i j #(assoc % :vec? (boolean checked))))

(defn- set-edn-map!
  "Blank removes K; valid EDN map sets it; anything else leaves it unchanged."
  [opts i j k s]
  (if (str/blank? s)
    (update-series! opts i j #(dissoc % k))
    (try
      (let [v (edn/read-string s)]
        (when (map? v)
          (update-series! opts i j #(assoc % k v))))
      (catch Exception _ nil))))

(defn- set-plot-height!
  [opts i s]
  (let [spec (read-spec opts)]
    (if (str/blank? s)
      (write-spec! (:code-a opts) (update-in spec [:plots i] dissoc :height))
      (when-let [n (parse-number s)]
        (when (pos? n)
          (write-spec! (:code-a opts)
                       (assoc-in spec [:plots i :height] (long n))))))))

;; ---------------------------------------------------------------------------
;; styling
;; ---------------------------------------------------------------------------

(def ^:private container-style
  {:overflow "auto" :max-height "60vh"
   :display "flex" :flex-direction "column" :gap "0.5rem"
   :font-family "system-ui, sans-serif" :font-size "0.8rem"})

(def ^:private input-style
  {:width "100%" :box-sizing "border-box" :padding "0.25rem 0.4rem"
   :border "1px solid #d1d5db" :border-radius "0.375rem" :font-size "0.8rem"})

(def ^:private select-style
  (assoc input-style :background "white"))

(def ^:private small-input-style
  (assoc input-style :width "6rem"))

(def ^:private button-style
  {:font-size "0.7rem" :padding "0.2rem 0.45rem" :cursor "pointer"
   :border "1px solid #d1d5db" :border-radius "0.375rem" :background "#f3f4f6"})

(def ^:private add-btn-style
  {:border "1px solid #2563eb" :background "#2563eb" :color "white"
   :cursor "pointer" :border-radius "0.375rem" :font-weight "700"
   :font-size "0.8rem" :padding "0.3rem 0.6rem"})

(def ^:private remove-btn-style
  (assoc button-style
         :border "1px solid #fca5a5" :background "#fef2f2" :color "#b91c1c"))

(def ^:private plot-card-style
  {:border "1px solid #e5e7eb" :border-radius "0.375rem"
   :padding "0.4rem" :background "#fafafa"})

(def ^:private series-card-style
  {:border "1px solid #d1d5db" :border-radius "0.375rem"
   :padding "0.4rem" :background "white"
   :display "flex" :flex-direction "column" :gap "0.35rem"})

(def ^:private field-row-style
  {:display "flex" :gap "0.35rem" :align-items "flex-end" :flex-wrap "wrap"})

(defn- field
  [label control]
  [:label {:style {:display "flex" :flex-direction "column" :gap "0.1rem"
                   :flex "1" :min-width "0" :color "#374151"}}
   [:span {:style {:font-size "0.7rem"}} label]
   control])

(def ^:private column-labels
  {:range       ["Low" "High"]
   :columnrange ["Low" "High"]
   :hlc         ["High" "Low" "Close"]
   :ohlc        ["Open" "High" "Low" "Close"]})

(defn- column-roles
  [type]
  (or (get column-labels type)
      (vec (repeat (get types/column-count type 1) "Column"))))

(defn- color-options
  [series]
  (let [current (:color series)
        extra   (when (and current (not (some #{current} color-palette)))
                  [current])]
    (into [""] (concat color-palette extra))))

;; ---------------------------------------------------------------------------
;; editor
;; ---------------------------------------------------------------------------

(defn- series-card
  [{:keys [opts spec i j series cols]}]
  (let [type     (:type series)
        opacity? (contains? #{:range :area :areaspline :columnrange} type)
        point?   (= :point type)
        flags?   (= :flags type)]
    [:div {:key j :style series-card-style}
     [:div {:style {:display "flex" :justify-content "space-between"
                    :align-items "center"}}
      [:span {:style {:font-size "0.7rem" :font-weight "600" :color "#6b7280"}}
       (str "Series " (inc j))]
      [:button {:type "button" :style remove-btn-style
                :data-on:click (h/action (delete-series! opts i j))}
       "Remove Series"]]

     [:div {:style field-row-style}
      (field "Type"
             [:select {:style select-style
                       :data-on:change
                       (h/action (set-type! opts i j (keyword $value)))}
              (for [t types/types]
                [:option {:value (name t) :selected (= t type)}
                 (name t)])])
      (field "Name"
             [:input {:type "text"
                      :value (or (:name series) "")
                      :style input-style
                      :data-on:change
                      (h/action (set-text! opts i j :name $value))}])]

     (field "Color"
            [:select {:style select-style
                      :data-on:change
                      (h/action (set-text! opts i j :color $value))}
             (for [c (color-options series)]
               [:option {:value c :selected (= c (:color series))}
                (if (str/blank? c) "\u2014" c)])])

     [:div {:style field-row-style}
      (for [[idx role] (map-indexed vector (column-roles type))]
        (let [current (resize-columns (:columns series) type)]
          [:label {:key idx
                   :style {:display "flex" :flex-direction "column"
                           :gap "0.1rem" :flex "1" :min-width "0"
                           :color "#374151"}}
           [:span {:style {:font-size "0.7rem"}} role]
           [:select {:style select-style
                     :data-on:change
                     (h/action (set-column! opts i j idx $value))}
            (for [c cols]
              [:option {:value (name c)
                        :selected (= c (nth current idx))}
               (name c)])]]))]

     (when opacity?
       (field "Opacity"
              [:input {:type "number" :min "0" :max "1" :step "0.05"
                       :value (if (some? (:opacity series))
                                (str (:opacity series)) "")
                       :style small-input-style
                       :data-on:change
                       (h/action (set-number! opts i j :opacity $value))}]))

     (when (= :ohlc type)
       [:div {:style field-row-style}
        (field "Mode"
               [:select {:style select-style
                         :data-on:change
                         (h/action (set-mode! opts i j $value))}
                (for [m [:ohlc :candle :candle-hollow]]
                  [:option {:value (name m)
                            :selected (= m (or (:mode series) :ohlc))}
                   (name m)])])
        (field "Bar color (EDN)"
               [:input {:type "text"
                        :value (if (some? (:barcolor series))
                                 (pr-str (:barcolor series)) "")
                        :placeholder "{:column :signal :color {true \"green-5\"}}"
                        :style input-style
                        :data-on:change
                        (h/action
                         (set-edn-map! opts i j :barcolor $value))}])])

     (when point?
       [:div {:style field-row-style}
        [:label {:style {:display "flex" :align-items "center" :gap "0.35rem"
                         :color "#374151" :flex "1"}}
         [:input {:type "checkbox"
                  :checked (boolean (:vec? series))
                  :style {:margin "0"}
                  :data-on:change
                  (h/action (set-vec?! opts i j $checked))}]
         "Expand sequences"]
        (field "Marker"
               [:input {:type "text"
                        :value (or (:marker series) "")
                        :placeholder "circle"
                        :style input-style
                        :data-on:change
                        (h/action (set-text! opts i j :marker $value))}])
        (field "Radius"
               [:input {:type "number" :min "0" :step "1"
                        :value (if (some? (:radius series))
                                 (str (:radius series)) "")
                        :style small-input-style
                        :data-on:change
                        (h/action (set-number! opts i j :radius $value))}])])

     (when flags?
       [:div {:style {:display "flex" :flex-direction "column" :gap "0.35rem"}}
        [:div {:style field-row-style}
         (field "Flags target"
                [:select {:style select-style
                          :data-on:change
                          (h/action
                           (set-text! opts i j :on-series $value))}
                 [:option {:value ""
                           :selected (nil? (:on-series series))} "\u2014"]
                 (for [id (sibling-ids spec i j)]
                   [:option {:value id
                             :selected (= id (:on-series series))}
                    id])])
         (field "Fill color"
                [:input {:type "text"
                         :value (or (:fillColor series) "")
                         :placeholder "black"
                         :style input-style
                         :data-on:change
                         (h/action
                          (set-text! opts i j :fillColor $value))}])]
        [:div {:style field-row-style}
         (field "Width"
                [:input {:type "number" :min "0" :step "1"
                         :value (if (some? (:width series))
                                  (str (:width series)) "")
                         :style small-input-style
                         :data-on:change
                         (h/action (set-number! opts i j :width $value))}])
         (field "Height"
                [:input {:type "number" :min "0" :step "1"
                         :value (if (some? (:height series))
                                  (str (:height series)) "")
                         :style small-input-style
                         :data-on:change
                         (h/action (set-number! opts i j :height $value))}])]
        (field "Shapes (EDN)"
               [:input {:type "text"
                        :value (if (some? (:v2style series))
                                 (pr-str (:v2style series)) "")
                        :placeholder "{:long \"url(...)\" :short \"url(...)\"}"
                        :style input-style
                        :data-on:change
                        (h/action
                         (set-edn-map! opts i j :v2style $value))}])])]))

(defn highcharts-timeseries-format-edit-ui
  "Edit a highcharts-timeseries spec bound to code-a.

   data  the chart rows (used to list selectable columns)
   opts  {:spec <current spec> :code-a <hyper cursor holding EDN text>}

   Every edit resets code-a to (pr-str spec); the spec popup persists it on
   Close.  Must be called inside a Hyper render context."
  [data opts]
  (let [spec (read-spec opts)
        cols (selectable-columns data spec)]
    [:div {:style container-style}
     [:div {:style {:display "flex" :justify-content "flex-end"}}
      [:button {:type "button" :style add-btn-style
                :data-on:click (h/action (add-plot! opts cols))}
       "+ Plot"]]

     (for [[i plot] (map-indexed vector (:plots spec))]
       (let [series (vec (:series plot))]
         [:details {:open true :key i :style plot-card-style}
          [:summary {:style {:font-size "0.75rem" :font-weight "600"
                             :cursor "pointer"}}
           (str "Plot " (inc i) " (" (count series) " series)")]
          [:div {:style {:display "flex" :flex-direction "column"
                         :gap "0.4rem" :margin-top "0.35rem"}}
           [:div {:style field-row-style}
            (field "Height"
                   [:input {:type "number" :min "40" :step "1"
                            :value (if (some? (:height plot))
                                     (str (:height plot)) "")
                            :placeholder (str default-plot-height)
                            :style small-input-style
                            :data-on:change
                            (h/action (set-plot-height! opts i $value))}])
            [:button {:type "button" :style add-btn-style
                      :data-on:click
                      (h/action (add-series! opts i cols))}
             "+ Series"]
            [:button {:type "button" :style remove-btn-style
                      :data-on:click
                      (h/action (delete-plot! opts i))}
             "Remove Plot"]]

           (for [[j s] (map-indexed vector series)]
             (series-card {:opts opts :spec spec :i i :j j
                           :series s :cols cols}))]]))]))