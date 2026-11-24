(ns itera.plot.pixi-timeseries.plot-format-ui
  "Editable charts spec for the pixi-timeseries viewer.

   The spec is the canonical map

     {:panes [<pane> ...]
      :main-pane-min-height <px>
      :aux-pane-height <px>}

   where pane 0 is the main pane and the rest are aux panes.  Each pane is a
   map of a y-axis scale and a vector of plot maps
   {:type <keyword> :col <keyword|vector> :color <string>}:

     {:main-pane-min-height 200
      :aux-pane-height 100
      :panes
      [{:yAxisScale :linear
        :plots [{:type :bar :col [:open :high :low :close]}
                {:type :line :col :close :color \"blue-5\"}]}
       {:yAxisScale :log
        :plots [{:type :line :col :close :color \"green-5\"}]}]}

   A legacy bare vector of panes is still read and normalized to the map
   shape.  Omitted/invalid pane heights fall back to 200 and 100 px
   (itera.plot.pixi-timeseries.pane-layout); the viewer uses them to fit as
   many aux panes as possible without shrinking the main pane below
   :main-pane-min-height.

   Old vector panes [[{:type ...} ...] ...] are still read and normalize to
   {:yAxisScale :linear :plots [...]}.  Bars carry no :color; normalize-spec
   strips one if present.  :point-drop uses a scalar :col plus an optional
   :base-value number; absent means 0 at render time and a blank number input
   removes the key rather than storing the default.  :background-color uses a
   single boolean :col plus :color; it paints a pane background band and has
   no effect on the y scale, and a non-boolean :col makes the plot render
   nothing (the viewer drops it server-side).  :signal uses a boolean or
   keyword :col:
   {:type :signal :col <bool|keyword column> :y-align <numeric column|nil>
    :size <4|6|8|10|12|14|16>}
   it has no :color.  :y-align nil puts the icon on the pane bottom; a
   numeric column centers it on that scaled value and includes the column in
   the pane's y bounds.

   `pixi-timeseries-format-edit-ui` renders the pane/plot editor over a code
   cursor; every edit resets that cursor to (pr-str spec) (always the map
   shape) and the spec popup persists it on Close.  Invalid EDN leaves the
   spec unchanged.

   The legacy :chartspec option editor is kept as a thin adapter over
   `charts-editor` so the demo algo page keeps working.  The option :spec
   carries the available columns (required) and, optionally, the colors; plot
   types always come from itera.plot.pixi-timeseries.plot-type/types.

   This namespace must not require itera.plot.pixi-timeseries.plot: the viewer
   requires this editor, not the other way around.  Must be called inside a
   Hyper render context."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hyper.core :as h]
            [itera.option.core :as options]
            [itera.util.color :as colors]
            [itera.plot.pixi-timeseries.pane-layout :as pane-layout]
            [itera.plot.pixi-timeseries.plot-type :as plot-type]
            ;; Loading these itera value namespaces (now from the itera
            ;; project) registers itera-table-info for the table shapes this
            ;; editor can receive (vectors/seqs and tech.ml datasets).
            [itera.value.dataset-table-info]
            [itera.value.protocol :as protocol]
            [itera.value.vector-table-info]))

(def ^:private field-style
  {:width "100%"})

(def ^:private button-style
  {:font-size "0.68rem" :padding "0.15rem 0.4rem" :cursor "pointer"
   :border "1px solid #d1d5db" :border-radius "0.2rem" :background "#f3f4f6"})

(def ^:private th-style
  {:text-align "left" :padding "0.3rem 0.4rem" :border-bottom "2px solid #e5e7eb"
   :background "#f9fafb" :color "#374151" :font-weight "600"
   :font-size "0.68rem" :text-transform "uppercase" :letter-spacing "0.03em"
   :box-sizing "border-box"})

(def ^:private td-style
  {:padding "0.3rem 0.4rem" :border-bottom "1px solid #eef2f7"
   :vertical-align "top" :box-sizing "border-box"})

(def ^:private select-row-style
  {:display "flex" :align-items "center" :gap "0.35rem"})

(def ^:private col-label-style
  {:flex "0 0 3.2rem" :font-size "0.68rem" :color "#374151"})

(def ^:private field-flex-style
  (assoc field-style :flex "1" :min-width "0"))

(def ^:private remove-base-style
  {:color "#b91c1c" :background "white" :border "1px solid #b91c1c"
   :border-radius "0.2rem" :cursor "pointer" :line-height "1"})

(def ^:private remove-plot-style
  (assoc remove-base-style :font-size "0.8rem" :padding "0.1rem 0.35rem"))

(def ^:private remove-pane-style
  (assoc remove-base-style
         :position "absolute" :top "0.3rem" :right "0.3rem" :z-index "1"
         :width "1.35rem" :height "1.35rem" :font-size "0.9rem" :padding "0"))

(def invalid-spec-block
  "Rendered in place of the viewer when a spec is not a charts map."
  [:div {:style {:padding "1rem" :color "#888"}}
   (str "Invalid pixi-timeseries spec: expected {:panes [<pane> ...]} "
        "with optional :main-pane-min-height and :aux-pane-height.")])

(def ^:private pane-scales #{:linear :log})

(defn- spec-panes
  "The pane vector of SPEC, accepting the legacy bare vector shape."
  [spec]
  (if (map? spec) (:panes spec) spec))

(defn valid-spec?
  "True when SPEC is the canonical map with a :panes vector, or the legacy
   bare vector of panes."
  [spec]
  (or (vector? spec)
      (and (map? spec) (vector? (:panes spec)))))

(defn- normalize-plot [plot]
  (if (map? plot)
    (if (contains? #{:bar :signal} (:type plot)) (dissoc plot :color) plot)
    {}))

(defn- normalize-pane
  "Normalize one pane to {:yAxisScale <:linear|:log> :plots [<plot> ...]},
   preserving unknown keys.  Old vector panes upgrade to :linear."
  [pane]
  (cond
    (vector? pane)
    {:yAxisScale :linear :plots (mapv normalize-plot pane)}

    (map? pane)
    (assoc pane
           :plots (if (vector? (:plots pane))
                    (mapv normalize-plot (:plots pane))
                    [])
           :yAxisScale (if (pane-scales (:yAxisScale pane))
                         (:yAxisScale pane)
                         :linear))

    :else
    {:yAxisScale :linear :plots []}))

(defn normalize-spec
  "Normalize SPEC into the canonical map
   {:panes [<pane> ...] :main-pane-min-height <px> :aux-pane-height <px>},
   or nil when SPEC is not a vector or {:panes <vector>}.

   Accepts the legacy bare pane vector, preserves unknown map keys, and
   defaults/clamps the pane heights to positive integers (see
   pane-layout/normalize-params).  A non-vector pane becomes an empty :linear
   pane and a non-map plot becomes an empty plot map."
  [spec]
  (when (valid-spec? spec)
    (let [panes (mapv normalize-pane (spec-panes spec))]
      (-> (if (map? spec) spec {})
          (assoc :panes panes)
          (merge (pane-layout/normalize-params spec))))))

(defn- table-column-info
  "Column name -> type for DATA; {} when DATA is nil or does not implement
   itera-table-info."
  [data]
  (if (and data (satisfies? protocol/itera-table-info data))
    (protocol/column-info data)
    {}))

(defn default-spec*
  "Smart default charts spec for the table DATA, in the canonical map shape:
   {:main-pane-min-height 200 :aux-pane-height 100 :panes [<pane> ...]}.

   One :bar pane when the data has numeric OHLC columns, otherwise a :line on
   :close / the first numeric non-time column; an empty table returns a map
   with :panes []."
  [data]
  (let [info  (table-column-info data)
        num?  (fn [k] (= :number (get info k)))
        panes (cond
                (empty? info)
                []

                (and (num? :open) (num? :high) (num? :low) (num? :close))
                [{:yAxisScale :linear
                  :plots [{:type :bar :col [:open :high :low :close]}]}]

                (num? :close)
                [{:yAxisScale :linear :plots [{:type :line :col :close}]}]

                :else
                (if-let [col (first (filter (fn [k]
                                              (and (num? k)
                                                   (not (contains? #{:date :epoch} k))))
                                            (keys info)))]
                  [{:yAxisScale :linear :plots [{:type :line :col col}]}]
                  []))]
    {:main-pane-min-height pane-layout/default-main-pane-min-height
     :aux-pane-height pane-layout/default-aux-pane-height
     :panes panes}))

(defn data-columns
  "Distinct column names across the table DATA, in column-info key order."
  [data]
  (vec (keys (table-column-info data))))

(defn- spec-plots
  "All plot maps in SPEC; accepts the canonical map and old vector panes."
  [spec]
  (mapcat (fn [pane] (if (map? pane) (:plots pane) pane))
          (or (spec-panes spec) [])))

(defn spec-columns
  "Column keywords already referenced by SPEC, whether scalar or vector :col."
  [spec]
  (->> (spec-plots spec)
       (keep :col)
       (mapcat #(if (vector? %) % [%]))
       (filter keyword?)
       distinct
       vec))

(defn selectable-columns
  "Data columns except time columns, plus columns already used by the spec."
  [data spec]
  (let [cols (remove #{:date :epoch} (keys (table-column-info data)))]
    (vec (distinct (concat cols (spec-columns spec))))))

(def ^:private signal-sizes [4 6 8 10 12 14 16])

(defn- signal-size-value [plot]
  (let [s (:size plot)]
    (if (some #{s} signal-sizes) s 8)))

(defn- boolean-columns [cols col-info]
  (filterv #(= :boolean (get col-info %)) cols))

(defn- numeric-columns [cols col-info]
  (if (empty? col-info)
    cols
    (filterv #(= :number (get col-info %)) cols)))

(defn- signal-columns [cols col-info]
  (let [cs (filterv #(contains? #{:boolean :keyword} (get col-info %)) cols)]
    (if (seq cs) cs cols)))

(defn- type-columns
  "Columns offered for plot type T; :background-color lists boolean columns,
   :signal lists boolean/keyword columns, falling back to all columns when the
   data has none of those types."
  [t cols col-info]
  (cond
    (= :background-color t)
    (let [b (boolean-columns cols col-info)] (if (seq b) b cols))

    (= :signal t) (signal-columns cols col-info)

    :else cols))

(defn palette*
  "Named colors, plus any colors already used by SPEC, sorted by name."
  [spec]
  (let [used (->> (spec-plots spec)
                  (keep :color)
                  (filter string?))]
    (vec (sort (distinct (concat (keys colors/colors) used))))))

(defn- read-spec
  "Current charts spec: valid EDN from code-a, else (:spec opts) when valid,
   else an empty canonical map.  Always returns the canonical map shape."
  [opts]
  (let [c      (:code-a opts)
        text   (try (when c @c) (catch Exception _ nil))
        parsed (try
                 (when (and (string? text) (not (str/blank? text)))
                   (edn/read-string text))
                 (catch Exception _ nil))]
    (cond
      (valid-spec? parsed)       (normalize-spec parsed)
      (valid-spec? (:spec opts)) (normalize-spec (:spec opts))
      :else                      (normalize-spec []))))

(defn- write-spec!
  [code-a spec]
  (reset! code-a (pr-str spec)))

(defn- columns
  "The option's required column keywords, or nil when missing/empty."
  [opt]
  (let [cols (get-in opt [:spec :columns])]
    (when (seq cols) (vec cols))))

(defn- palette
  "The option's optional color names, stringified and sorted by name,
   defaulting to itera.util.color/colors."
  [opt]
  (let [cs (get-in opt [:spec :colors])]
    (if (seq cs)
      (vec (sort (map str cs)))
      (vec (sort (keys colors/colors))))))

(defn- default-plot [cols pal]
  {:type :line :col (first cols) :color (first pal)})

(defn- ohlc-default
  "The default 4-column bar layout: [:open :high :low :close] when all exist in
   cols, otherwise the first column repeated four times."
  [cols]
  (let [want [:open :high :low :close]]
    (if (every? (set cols) want) want (vec (repeat 4 (first cols))))))

(defn- ->col-pair
  "Normalize a :col into a [c1 c2] pair, defaulting to the first column."
  [col cols]
  (let [d (first cols)
        v (if (vector? col) (vec col) [col])]
    [(or (first v) d) (or (second v) d)]))

(defn- ->col-4
  "Normalize a :col into a 4-vector for :bar, filling gaps with OHLC defaults."
  [col cols]
  (let [d (ohlc-default cols)
        v (if (vector? col) (vec col) [col])]
    [(or (nth v 0 nil) (nth d 0))
     (or (nth v 1 nil) (nth d 1))
     (or (nth v 2 nil) (nth d 2))
     (or (nth v 3 nil) (nth d 3))]))

(defn- set-plot-type
  "Change a plot's :type, normalizing :col to a 2-vector for :range, a 4-vector
   for :bar, and a single keyword otherwise.  Bars and signals have no color;
   other types use the first palette color when none is set.  :background-color
   keeps a boolean :col, else falls back to the first boolean column, then the
   first column.  :signal keeps an existing :y-align (including explicit nil)
   and otherwise defaults it to :close when that numeric column exists, and
   normalizes :size to one of the supported signal sizes."
  [plot t cols pal col-info]
  (let [col (:col plot)
        col (if (nil? col) (first cols) col)]
    (cond
      (= t :range) (assoc plot :type :range :col (->col-pair col cols)
                          :color (or (:color plot) (first pal)))
      (= t :bar)   (-> plot
                       (assoc :type :bar :col (->col-4 col cols))
                       (dissoc :color))
      (= t :background-color)
      (let [col (if (vector? col) (first col) col)
            b   (boolean-columns cols col-info)]
        (assoc plot :type :background-color
               :col (if (= :boolean (get col-info col))
                      col
                      (or (first b) (first cols) col))
               :color (or (:color plot) (first pal))))
      (= t :signal)
      (let [col    (if (vector? col) (first col) col)
            close? (if (seq col-info)
                     (= :number (get col-info :close))
                     (boolean (some #{:close} cols)))
            out    (-> plot
                       (assoc :type :signal
                              :col col
                              :size (signal-size-value plot))
                       (dissoc :color))]
        (if (contains? plot :y-align)
          out
          (assoc out :y-align (when close? :close))))
      :else        (assoc plot :type t
                          :col (if (vector? col) (first col) col)
                          :color (or (:color plot) (first pal))))))

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

(defn- set-base-value!
  "Set a :point-drop plot's :base-value; blank removes the key (render default
   0), invalid input leaves the spec unchanged."
  [update* path i j s]
  (update* path
           (fn [cs]
             (if (str/blank? s)
               (update-in cs [:panes i :plots j] dissoc :base-value)
               (if-let [n (parse-number s)]
                 (assoc-in cs [:panes i :plots j :base-value] n)
                 cs)))))

(defn- plot-row
  "One plot's editor table row."
  [{:keys [path update* cols pal types col-info i j plot]}]
  (let [plot   (if (map? plot) plot {})
        t      (:type plot)
        col    (:col plot)
        range? (= :range t)
        bar?   (= :bar t)
        drop?  (= :point-drop t)
        signal? (= :signal t)
        ccol   (->col-pair col cols)
        bcol   (->col-4 col cols)]
    [:tr
     [:td {:style td-style}
      [:div {:style {:display "flex" :flex-direction "column" :gap "0.15rem"}}
       [:span {:style {:font-size "0.65rem" :color "#6b7280"}}
        (str "Plot " (inc (or j 0)))]
       [:select {:style field-style
                 :data-on:change
                 (h/action
                  (update* path
                           (fn [cs]
                             (update-in cs [:panes i :plots j] set-plot-type
                                        (keyword $value) cols pal col-info))))}
        (for [tt types]
          [:option {:value (name tt) :selected (= tt t)} (name tt)])]]]

     [:td {:style td-style}
      (cond
        bar?
        [:div {:style {:display "flex" :flex-direction "column" :gap "0.2rem"}}
         [:div {:style select-row-style}
          [:span {:style col-label-style} "Open:"]
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (let [cc (->col-4 (get-in cs [:panes i :plots j :col]) cols)]
                                  (assoc-in cs [:panes i :plots j :col]
                                            (assoc cc 0 (keyword $value)))))))}
           (for [c cols]
             [:option {:value (name c) :selected (= c (nth bcol 0))} (name c)])]]
         [:div {:style select-row-style}
          [:span {:style col-label-style} "High:"]
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (let [cc (->col-4 (get-in cs [:panes i :plots j :col]) cols)]
                                  (assoc-in cs [:panes i :plots j :col]
                                            (assoc cc 1 (keyword $value)))))))}
           (for [c cols]
             [:option {:value (name c) :selected (= c (nth bcol 1))} (name c)])]]
         [:div {:style select-row-style}
          [:span {:style col-label-style} "Low:"]
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (let [cc (->col-4 (get-in cs [:panes i :plots j :col]) cols)]
                                  (assoc-in cs [:panes i :plots j :col]
                                            (assoc cc 2 (keyword $value)))))))}
           (for [c cols]
             [:option {:value (name c) :selected (= c (nth bcol 2))} (name c)])]]
         [:div {:style select-row-style}
          [:span {:style col-label-style} "Close:"]
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (let [cc (->col-4 (get-in cs [:panes i :plots j :col]) cols)]
                                  (assoc-in cs [:panes i :plots j :col]
                                            (assoc cc 3 (keyword $value)))))))}
           (for [c cols]
             [:option {:value (name c) :selected (= c (nth bcol 3))} (name c)])]]]

        range?
        [:div {:style {:display "flex" :flex-direction "column" :gap "0.2rem"}}
         [:div {:style select-row-style}
          [:span {:style col-label-style} "Col 1:"]
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (let [cc (->col-pair (get-in cs [:panes i :plots j :col]) cols)]
                                  (assoc-in cs [:panes i :plots j :col]
                                            (assoc cc 0 (keyword $value)))))))}
           (for [c cols]
             [:option {:value (name c) :selected (= c (first ccol))} (name c)])]]
         [:div {:style select-row-style}
          [:span {:style col-label-style} "Col 2:"]
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (let [cc (->col-pair (get-in cs [:panes i :plots j :col]) cols)]
                                  (assoc-in cs [:panes i :plots j :col]
                                            (assoc cc 1 (keyword $value)))))))}
           (for [c cols]
             [:option {:value (name c) :selected (= c (second ccol))} (name c)])]]]

        signal?
        (let [display-cols (type-columns t cols col-info)
              num-cols     (numeric-columns cols col-info)
              ya           (:y-align plot)]
          [:div {:style {:display "flex" :flex-direction "column" :gap "0.2rem"}}
           [:div {:style select-row-style}
            [:span {:style col-label-style} "Column:"]
            [:select {:style field-flex-style
                      :data-on:change
                      (h/action
                       (update* path
                                (fn [cs]
                                  (assoc-in cs [:panes i :plots j :col] (keyword $value)))))}
             (for [c display-cols]
               [:option {:value (name c) :selected (= c col)} (name c)])]]
           [:div {:style select-row-style}
            [:span {:style col-label-style} "Y align:"]
            [:select {:style field-flex-style
                      :data-on:change
                      (h/action
                       (update* path
                                (fn [cs]
                                  (if (str/blank? $value)
                                    (assoc-in cs [:panes i :plots j :y-align] nil)
                                    (assoc-in cs [:panes i :plots j :y-align]
                                              (keyword $value))))))}
             [:option {:value "" :selected (nil? ya)} "\u2014"]
             (for [c num-cols]
               [:option {:value (name c) :selected (= c ya)} (name c)])]]])

        :else
        (let [display-cols (type-columns t cols col-info)]
          [:div {:style select-row-style}
           [:span {:style col-label-style} "Column:"]
           [:select {:style field-flex-style
                     :data-on:change
                     (h/action
                      (update* path
                               (fn [cs]
                                 (assoc-in cs [:panes i :plots j :col] (keyword $value)))))}
            (for [c display-cols]
              [:option {:value (name c) :selected (= c col)} (name c)])]]))]

     [:td {:style td-style}
      [:div {:style {:display "flex" :flex-direction "column" :gap "0.25rem"}}
       [:div {:style {:display "flex" :justify-content "flex-end"
                      :align-items "center" :gap "0.3rem"}}
        (when signal?
          [:span {:style col-label-style} "Size:"])
        (when signal?
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (if-let [n (parse-number $value)]
                                  (assoc-in cs [:panes i :plots j :size] n)
                                  cs))))}
           (for [s signal-sizes]
             [:option {:value (str s)
                       :selected (= s (signal-size-value plot))}
              (str s)])])
        (when-not (or bar? signal?)
          [:select {:style field-flex-style
                    :data-on:change
                    (h/action
                     (update* path
                              (fn [cs]
                                (assoc-in cs [:panes i :plots j :color] $value))))}
           (for [c pal]
             [:option {:value c :selected (= c (:color plot))} c])])
        [:button {:type "button" :title "Remove plot" :aria-label "Remove plot"
                  :style remove-plot-style
                  :data-on:click
                  (h/action
                   (update* path
                            (fn [cs]
                              (update-in cs [:panes i :plots]
                                         (fn [p]
                                           (let [p (vec (or p []))]
                                             (vec (concat (subvec p 0 j)
                                                          (subvec p (inc j))))))))))} 
         "×"]]
       (when drop?
         [:div {:style select-row-style}
          [:span {:style col-label-style} "Base:"]
          [:input {:type "number" :step "any"
                   :value (str (or (:base-value plot) 0))
                   :style field-flex-style
                   :data-on:change
                   (h/action (set-base-value! update* path i j $value))}]])]]]))

(defn- plot-table
  "The plots of pane I as a 3-column table: Type, Column(s), Opts."
  [{:keys [path update* cols pal types col-info i plots]}]
  [:div {:style {:overflow-x "auto"}}
   [:table {:style {:width "100%" :border-collapse "collapse"
                    :table-layout "fixed" :font-size "0.68rem"}}
    [:colgroup
     [:col {:style {:width "20em"}}]
     [:col {:style {:width "20em"}}]
     [:col]]
    [:thead
     [:tr
      [:th {:style th-style} "Type"]
      [:th {:style th-style} "Column(s)"]
      [:th {:style th-style} "Opts"]]]
    [:tbody
     (map-indexed
      (fn [j plot]
        (plot-row {:path path :update* update* :cols cols :pal pal
                   :types types :col-info col-info :i i :j j :plot plot}))
      plots)]]])

(defn- set-pane-height!
  "Set a top-level pane-height key from a numeric input; blank or non-positive
   integer input leaves the spec unchanged."
  [update* path k s]
  (update* path
           (fn [cs]
             (let [n (parse-number s)]
               (if (and (integer? n) (pos? n))
                 (assoc cs k n)
                 cs)))))

(defn charts-editor
  "Charts spec editor body.

   charts   the canonical spec map
            {:panes [<pane> ...] :main-pane-min-height <px>
             :aux-pane-height <px>}; a legacy pane vector is normalized
   path     path passed through to update*
   update*  two-arg (fn [path f]) read-modify-write of the spec value
   cols     selectable column keywords
   pal      selectable color names
   types    plot type keywords
   col-info optional column name -> type map (narrows :background-color to
            boolean columns)

   Every edit writes the canonical map shape."
  [{:keys [charts path update* cols pal types col-info]}]
  (let [cols         (vec (or cols []))
        pal          (vec (sort (distinct (or pal []))))
        types        (vec (or types plot-type/types))
        normalize    (fn [cs] (or (normalize-spec cs) (normalize-spec [])))
        spec         (normalize charts)
        update*      (fn [p f] (update* p (fn [cs] (f (normalize cs)))))
        add-pane     (fn [s]
                       (update s :panes conj
                               {:yAxisScale :linear
                                :plots [(default-plot cols pal)]}))
        remove-pane  (fn [s i]
                       (let [ps (vec (:panes s))]
                         (assoc s :panes
                                (vec (concat (subvec ps 0 i)
                                             (subvec ps (inc i)))))))
        add-plot     (fn [s i]
                       (update-in s [:panes i :plots] (fnil conj [])
                                  (default-plot cols pal)))
        height-field (fn [k label]
                       [:label {:style {:display "flex" :flex-direction "column"
                                        :font-size "0.68rem" :color "#374151"
                                        :gap "0.1rem" :flex "1" :min-width "0"}}
                        label
                        [:input {:type "number" :min "1" :step "1"
                                 :value (str (get spec k))
                                 :style field-style
                                 :data-on:change
                                 (h/action
                                  (set-pane-height! update* path k $value))}]])]
    [:div {:style {:display "flex" :flex-direction "column" :gap "0.5rem"}}
     [:div {:style {:display "flex" :gap "0.35rem"}}
      (height-field :main-pane-min-height "Main pane min height (px)")
      (height-field :aux-pane-height "Aux pane height (px)")]
     (mapv
      (fn [[i pane]]
        (let [plots (vec (or (:plots pane) []))
              scale (or (:yAxisScale pane) :linear)]
          [:div {:style {:position "relative"}}
           [:details {:open true
                      :style {:border "1px solid #e5e7eb"
                              :border-radius "0.25rem"
                              :padding "0.35rem" :background "#fafafa"}}
            [:summary {:style {:font-size "0.75rem" :font-weight "600"
                               :cursor "pointer" :padding-right "2rem"}}
             (str "Pane " (inc i) " (" (count plots) " plots)")]
            [:div {:style {:display "flex" :flex-direction "column"
                           :gap "0.4rem" :margin-top "0.35rem"}}
             [:div {:style {:display "flex" :align-items "center" :gap "0.35rem"
                            :font-size "0.68rem" :color "#374151"}}
              [:span "Y axis"]
              [:select {:data-on:change
                        (h/action
                         (update* path
                                  (fn [cs]
                                    (assoc-in cs [:panes i :yAxisScale]
                                              (keyword $value)))))}
               (for [s [:linear :log]]
                 [:option {:value (name s) :selected (= s scale)}
                  (str/capitalize (name s))])]]
             (plot-table {:path path :update* update* :cols cols :pal pal
                          :types types :col-info col-info :i i :plots plots})
             [:div
              [:button {:type "button" :style button-style
                        :data-on:click
                        (h/action (update* path (fn [cs] (add-plot cs i))))}
               "+ Plot"]]]]
           [:button {:type "button" :title "Remove pane" :aria-label "Remove pane"
                     :style remove-pane-style
                     :data-on:click
                     (h/action (update* path (fn [cs] (remove-pane cs i))))}
            "×"]]))
      (map-indexed vector (:panes spec)))
     [:div {:style {:display "flex" :justify-content "flex-start"}}
      [:button {:type "button" :style button-style
                :data-on:click (h/action (update* path add-pane))}
       "+ Pane"]]]))

(defn pixi-timeseries-format-edit-ui
  "Edit a pixi-timeseries charts spec bound to code-a.

   data  the plot table (used to list selectable columns via itera-table-info)
   opts  {:spec <current spec map or legacy pane vector>
          :code-a <hyper cursor holding EDN text>}

   Every edit resets code-a to (pr-str spec) in the canonical map shape; the
   spec popup persists it on Close.  Must be called inside a Hyper render
   context."
  [data opts]
  (let [spec    (read-spec opts)
        code-a  (:code-a opts)
        update* (fn [_path f] (write-spec! code-a (f (read-spec opts))))]
    (charts-editor {:charts spec
                    :path [:charts]
                    :update* update*
                    :cols (selectable-columns data spec)
                    :pal (palette* spec)
                    :types plot-type/types
                    :col-info (table-column-info data)})))

(defmethod options/editor :chartspec [opt value set-fn update-fn _group]
  (let [path    (options/norm-path (:path opt))
        cols    (columns opt)
        update* (or update-fn (fn [p f] (set-fn p (f value))))]
    (if (nil? cols)
      [:div {:style {:color "#b91c1c" :font-size "0.75rem"}}
       "ChartSpec: the option :spec must provide a non-empty :columns vector."]
      (charts-editor {:charts value
                      :path path
                      :update* update*
                      :cols cols
                      :pal (palette opt)
                      :types plot-type/types}))))