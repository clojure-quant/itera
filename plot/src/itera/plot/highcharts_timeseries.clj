(ns itera.plot.highcharts-timeseries
  "Multi-pane Highstock timeseries viewer with an editable plot/series spec.

   The public `highcharts-timeseries` fn takes only the row `data`:
     (highcharts-timeseries rows)

   Each row is a map with :epoch (long ms) or :date (Instant / ISO string).
   The chart is stacked vertically: the spec has one plot per Y axis and each
   plot contains one or more series.

   Editable spec:
     {:plots [{:height <px>
               :series [{:type <keyword>          ; see supported-types
                         :columns [<keyword> ...]  ; 1-4 columns by :type
                         :name \"...\"              ; optional series/legend name
                         :color \"blue-5\"           ; named or CSS color
                         :opacity <number>          ; range/area/areaspline/columnrange
                         :mode :candle              ; :ohlc
                         :barcolor {:column <kw> :color {<value> <color>}}
                         :vec? true :marker \"square\" :radius 5 ; :point
                         :v2style {<value> \"url(...)\"}
                         :on-series \"ohlc\"         ; :flags target series id
                         :fillColor \"...\" :width 10 :height 10} ...]} ...]}

   A legacy singular :column is normalized to :columns.  Series ids are
   (or :id :name (str first column) (name :type)); a :flags series targets
   the first :ohlc/:hlc series in its plot unless :on-series names a sibling.

   With no spec, `default-spec*` picks OHLC + volume when the rows have
   :open/:high/:low/:close, otherwise a line on :close / the first numeric
   non-time column, otherwise no plots.

   The returned record implements both itera.value.protocol (renders the
   default spec) and itera.value.protocol (the plot/series spec).  The
   editor lives in itera.plot.highcharts-timeseries-format-ui."
  (:require [itera.plot.highcharts :as highcharts]
            [itera.plot.highcharts-timeseries-format-ui :as fmt]
            [itera.plot.highcharts-timeseries-types :as types]
            [itera.util.color :as color]
            [itera.value.protocol :as protocol])
  (:import (java.time Instant LocalDate LocalDateTime ZonedDateTime)))

(def default-plot-height
  "Plot height (px) used when a plot does not set :height."
  fmt/default-plot-height)

(def supported-types
  "All series types the viewer can render."
  types/types)

(def ^:private axes-default
  {:resize      {:enabled true}
   :startOnTick false
   :endOnTick   false
   :margin      0
   :lineWidth   2
   :labels      {:align "right" :x -3}})

(def ^:private default-template
  {:xAxis    [{:crosshair     {:snap true}
               :allowOverlap  true
               :scrollbar     {:enabled false :showFull false}
               :zIndex        4000}]
   :tooltip  {:style         {:width "200px"}
              :valueDecimals 4
              :shared        true}
   :boost    false
   :chart    {:height      1000
              :margin      [0 0 0 0]
              :zooming     {:key "alt" :type "x"}
              :panning     {:enabled true :type "x"}
              :panKey      "ctrl"
              :animation   false
              :time        {:useUTC true}
              :styledMode  false}
   :plotOptions {:series      {:animation 0
                               :states    {:inactive {:opacity 1}}}
                 :candlestick {:color       (color/set-color "red-5")
                               :lineColor   (color/set-color "red-5")
                               :upColor     (color/set-color "blue-5")
                               :upLineColor (color/set-color "blue-5")}}
   :credits  {:enabled false}
   :accessibility {:enabled false}
   :navigator     {:enabled false}
   :rangeSelector {:enabled false}})

(def ^:private default-flag-styles
  {:long  "url(/itera/plot/flags/arrow-up.svg)"
   true   "flags"
   :short "url(/itera/plot/flags/arrow-down.svg)"})

(defn- ->epoch [v]
  (cond
    (instance? Instant v)       (.toEpochMilli ^Instant v)
    (instance? ZonedDateTime v) (.toEpochMilli (.toInstant ^ZonedDateTime v))
    (instance? LocalDateTime v) (.toEpochMilli (.toInstant ^LocalDateTime v java.time.ZoneOffset/UTC))
    (instance? LocalDate v)     (.toEpochMilli (.toInstant (.atStartOfDay ^LocalDate v) java.time.ZoneOffset/UTC))
    (number? v)                 (long v)
    (string? v)                 (try (.toEpochMilli (Instant/parse v))
                                     (catch Exception _ nil))
    :else                       nil))

(defn- ensure-epoch [row]
  (if (:epoch row)
    row
    (assoc row :epoch (->epoch (:date row)))))

(defn- series-id [series]
  (or (:id series)
      (:name series)
      (some-> (:columns series) first str)
      (some-> (:type series) name)))

(defn- explicit-target
  [v]
  (cond
    (string? v)  v
    (keyword? v) (name v)
    (some? v)    (str v)
    :else        nil))

(defn- flags-target
  [descs series]
  (or (explicit-target (:on-series series))
      (some (fn [s]
              (when (and (= (:axis s) (:axis series))
                         (contains? #{:ohlc :hlc} (:type s)))
                (series-id s)))
            descs)))

(defn- attach-flags-targets
  [descs]
  (mapv (fn [series]
          (if (= :flags (:type series))
            (assoc series :on-series (flags-target descs series))
            series))
        descs))

(defn- series-data
  [{:keys [type vec? barcolor v2style] :as series} rows]
  (let [cols (:columns series)
        col  (first cols)]
    (case type
      :ohlc
      (mapv (fn [row]
              (let [p {:x     (:epoch row)
                       :open  (get row (nth cols 0 nil))
                       :high  (get row (nth cols 1 nil))
                       :low   (get row (nth cols 2 nil))
                       :close (get row (nth cols 3 nil))}]
                (if barcolor
                  (let [{:keys [column color]} barcolor
                        v (get row column)]
                    (if-let [c (get color v)]
                      (assoc p :color (color/set-color c))
                      p))
                  p)))
            rows)

      :hlc
      (mapv (fn [row]
              [(:epoch row)
               (get row (nth cols 0 nil))
               (get row (nth cols 1 nil))
               (get row (nth cols 2 nil))])
            rows)

      (:range :columnrange)
      (mapv (fn [row]
              (if (>= (count cols) 2)
                [(:epoch row)
                 (get row (nth cols 0))
                 (get row (nth cols 1))]
                (let [v (get row col)]
                  [(:epoch row) (first v) (second v)])))
            rows)

      :flags
      (let [v2style (or v2style default-flag-styles)]
        (->> rows
             (filter (fn [row] (contains? v2style (get row col))))
             (mapv (fn [row]
                     (let [v (get row col)]
                       {:x     (:epoch row)
                        :title (str v)
                        :shape (get v2style v)
                        :text  (str "col: " col " val: " v)})))))

      :point
      (if vec?
        (->> rows
             (mapcat (fn [row]
                       (let [vs (get row col)]
                         (when (sequential? vs)
                           (map (fn [p] [(:epoch row) p]) vs)))))
             vec)
        (mapv (fn [row] [(:epoch row) (get row col)]) rows))

      (mapv (fn [row] [(:epoch row) (get row col)]) rows))))

(defn- series-spec
  [{:keys [axis opacity mode marker radius fillColor width height on-series] :as series}]
  (let [type     (:type series)
        id       (series-id series)
        resolved (color/set-color (or (:color series) "blue"))
        base     {:id           id
                  :yAxis        axis
                  :name         (or (:name series) id)
                  :zIndex       1000
                  :animation    false
                  :dataGrouping {:enabled false}}]
    (merge base
           (case type
             :flags
             (cond-> {:type         "flags"
                      :shape        "squarepin"
                      :fillColor    "rgba(255, 255, 255, .4)"
                      :allowOverlapX true
                      :zIndex       9999
                      :onKey        "close"}
               (some? on-series) (assoc :onSeries on-series)
               (some? fillColor) (assoc :fillColor fillColor)
               (some? width)     (assoc :width width)
               (some? height)    (assoc :height height))

             :step
             {:type  "line"
              :step  true
              :color resolved}

             :line
             {:type  "line"
              :color resolved}

             :spline
             {:type  "spline"
              :color resolved}

             :area
             {:type        "area"
              :color       resolved
              :fillOpacity (or opacity 0.75)}

             :areaspline
             {:type        "areaspline"
              :color       resolved
              :fillOpacity (or opacity 0.75)}

             :column
             {:type  "column"
              :color resolved}

             :columnrange
             (cond-> {:type  "columnrange"
                      :color resolved}
               (some? opacity) (assoc :opacity opacity))

             :range
             {:type        "arearange"
              :color       resolved
              :fillOpacity (or opacity 0.75)}

             :hlc
             {:type  "hlc"
              :color resolved}

             :ohlc
             {:type (case (keyword mode)
                      :candle        "candlestick"
                      :candle-hollow "hollowcandlestick"
                      "ohlc")}

             :point
             {:type   "scatter"
              :marker {:enabled   true
                       :symbol    (or marker "circle")
                       :radius    (or radius 2)
                       :fillColor resolved}}

             {:type  "line"
              :color resolved}))))

(defn- plot-height
  [plot]
  (let [height (:height plot)]
    (if (and (number? height) (pos? height))
      height
      default-plot-height)))

(defn- y-axes
  [plots]
  (let [n (count plots)]
    (loop [i 0 top 0 axes []]
      (if (< i n)
        (let [height (plot-height (nth plots i))]
          (recur (inc i)
                 (+ top height)
                 (conj axes (assoc axes-default
                                   :top top
                                   :height height
                                   :resize {:enabled (< i (dec n))}))))
        axes))))

(defn- plot-descs
  [plots]
  (vec
   (mapcat (fn [i plot]
             (mapv (fn [series] (assoc series :axis i))
                   (:series plot)))
           (range) plots)))

(defn- numeric-column?
  [rows k]
  (boolean (some #(number? (get % k)) rows)))

(defn- rows-have-ohlc?
  [rows]
  (every? #(numeric-column? rows %) [:open :high :low :close]))

(defn- first-numeric-column
  [rows]
  (let [row (first rows)]
    (or (when (numeric-column? rows :close) :close)
        (some (fn [k]
                (when (and (not (contains? #{:epoch :date} k))
                           (number? (get row k)))
                  k))
              (keys row)))))

(defn default-spec*
  "Smart default spec for ROWS: OHLC + volume when the data has OHLC keys,
   otherwise a line on :close / the first numeric non-time column, otherwise
   {:plots []}."
  [rows]
  (cond
    (empty? rows)
    {:plots []}

    (rows-have-ohlc? rows)
    {:plots (cond-> [{:height 400
                      :series [{:type    :ohlc
                                :columns [:open :high :low :close]
                                :mode    :candle}]}]
              (numeric-column? rows :volume)
              (conj {:height 120
                     :series [{:type    :column
                               :columns [:volume]}]}))}

    :else
    (if-let [col (first-numeric-column rows)]
      {:plots [{:height 400
                :series [{:type :line :columns [col]}]}]}
      {:plots []})))

(defn build-options
  "Build the Highcharts options map from a normalized SPEC and ROWS.
   Returns only the options: no :style / :class host passthrough."
  [spec rows]
  (let [rows   (mapv ensure-epoch rows)
        plots  (:plots spec)
        descs  (-> (plot-descs plots) attach-flags-targets)
        series (mapv (fn [series]
                       (assoc (series-spec series)
                              :data (series-data series rows)))
                     descs)
        total  (reduce + 0 (map plot-height plots))]
    (-> default-template
        (assoc :yAxis (y-axes plots))
        (assoc :series series)
        (assoc-in [:chart :height] total))))

(defn render
  "Render a normalized SPEC over ROWS as an itera-highstock hiccup element.
   The host is full width and as tall as the summed plot heights."
  [spec rows]
  (let [options (build-options spec rows)
        height  (get-in options [:chart :height])
        style   {:width "100%" :height (str height "px")}]
    (protocol/to-hiccup (highcharts/highstock (assoc options :style style)))))

(def no-rows-block
  [:div {:style {:padding "1rem" :color "#888"}}
   "No rows in this chart."])

(def invalid-spec-block
  [:div {:style {:padding "1rem" :color "#888"}}
   "Invalid highcharts-timeseries spec: expected {:plots [{:height <px> :series [<series> ...]} ...]}"])

(defrecord HighchartsTimeseries [data]
  protocol/itera-convertable
  (to-hiccup [_]
    (if (empty? data)
      no-rows-block
      (render (default-spec* data) data)))

  protocol/itera-spec-plot
  (to-formatted-hiccup [_ spec _window]
    (if (empty? data)
      no-rows-block
      (if-let [spec (fmt/normalize-spec (or spec (default-spec* data)))]
        (render spec data)
        invalid-spec-block)))
  (default-spec [_]
    (default-spec* data))
  (spec-editor [v spec code-a]
    (fmt/highcharts-timeseries-format-edit-ui
     (:data v)
     {:spec spec :code-a code-a})))

(defn highcharts-timeseries
  "Return a highcharts-timeseries viewer over ROWS (a vector of row maps with
   :epoch or :date)."
  [rows]
  (->HighchartsTimeseries (vec (or rows []))))