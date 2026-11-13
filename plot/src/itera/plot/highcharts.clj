(ns itera.plot.highcharts
  "Highcharts / Highstock viewers as Hyper client components.

   `highchart` renders via Highcharts.chart (the main `highcharts`
   module) and `highstock` via Highcharts.stockChart (the `highcharts/highstock`
   subpath, which includes the stock chart and its extra modules).

   Both take the Highcharts options map directly, plus `:style` / `:class` host
   passthrough, e.g.:

     (highchart {:chart {:type \"line\"}
                 :series [{:name \"A\" :data [1 2 3]}]
                 :style {:width \"100%\" :height \"400px\"}})

   `highstock-ds` builds a multi-pane Highstock chart from a vector of row maps
   and a compact chart spec (see its docstring).

   The stylesheet is served locally from the plot sub-project's resources."
  (:require [hyper.core :as h]
            [itera.plot.color :as color])
  (:import (java.time Instant LocalDate LocalDateTime ZonedDateTime)))

(def head
  [[:link {:rel  "stylesheet"
           :href "/itera/plot/highcharts/css/highcharts.css"}]])

(h/defc itera-highchart
  {:require [["https://esm.sh/highcharts@11.4.8" :as hc]
             ["https://esm.sh/highcharts@11.4.8/highcharts-more" :as hcmore]
             ["https://esm.sh/highcharts@11.4.8/modules/annotations" :as annotations]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [H hc/default
          el (.querySelector root "div")]
      (hcmore/default H)
      (annotations/default H)
      (set! (.-chart ctx) (.chart H el options))))
  (update [root]
    (when-let [c (.-chart ctx)] (.destroy c))
    (let [H hc/default
          el (.querySelector root "div")]
      (hcmore/default H)
      (annotations/default H)
      (set! (.-chart ctx) (.chart H el options))))
  (unmount [_]
    (when-let [c (.-chart ctx)] (.destroy c))))

(h/defc itera-highstock
  {:require [["https://esm.sh/highcharts@11.4.8/highstock" :as hs]
             ["https://esm.sh/highcharts@11.4.8/highcharts-more" :as hcmore]]}
  [{:keys [options]}]
  (render
   [:div {:style "width:100%;height:100%"}])
  (mount [root]
    (let [H hs/default
          el (.querySelector root "div")]
      (hcmore/default H)
      (set! (.-chart ctx) (.stockChart H el options))))
  (update [root]
    (when-let [c (.-chart ctx)] (.destroy c))
    (let [H hs/default
          el (.querySelector root "div")]
      (hcmore/default H)
      (set! (.-chart ctx) (.stockChart H el options))))
  (unmount [_]
    (when-let [c (.-chart ctx)] (.destroy c))))

(defn highchart
  [{:keys [style class] :as opts}]
  (itera-highchart (cond-> {:options (dissoc opts :style :class)}
                     style (assoc :style style)
                     class (assoc :class class))))

(defn highstock
  [{:keys [style class] :as opts}]
  (itera-highstock (cond-> {:options (dissoc opts :style :class)}
                     style (assoc :style style)
                     class (assoc :class class))))

;; --- highstock-ds -----------------------------------------------------------

(def ^:private axes-default
  {:resize      {:enabled true}
   :startOnTick false
   :endOnTick   false
   :margin      0
   :lineWidth   2
   :labels      {:align "right" :x -3}})

(def ^:private default-template
  {:xAxis    [{:crosshair   {:snap true}
               :title       "bongo27"
               :allowOverlap true
               :scrollbar   {:enabled false :showFull false}
               :zIndex      4000}]
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

(defn- pane->series [idx pane]
  (map (fn [[col spec]]
         (cond
           (map? spec)     (merge {:axis idx :column col}
                                  (update spec :type #(if (string? %) (keyword %) %)))
           (keyword? spec) {:axis idx :column col :type spec}
           (string? spec)  {:axis idx :column col :type (keyword spec)}
           :else           {:axis idx :column col :type :line}))
       pane))

(defn- chart->series [charts]
  (->> (map-indexed pane->series charts)
       (reduce concat)
       vec))

(defn- add-flags-target [descs]
  (mapv (fn [d]
          (if (= :flags (:type d))
            (let [target (some (fn [x]
                                 (when (and (= (:axis x) (:axis d))
                                            (= :ohlc (:type x)))
                                   (:column x)))
                               descs)]
              (assoc d :on-series (or target :bar)))
            d))
        descs))

(defn- series-data [{:keys [type column vec? barcolor v2style] :as _desc} rows]
  (case type
    :ohlc
    (mapv (fn [row]
            (let [p {:x     (:epoch row)
                     :open  (:open row)
                     :high  (:high row)
                     :low   (:low row)
                     :close (:close row)}]
              (if barcolor
                (let [{:keys [column color]} barcolor
                      v (get row column)]
                  (if-let [c (get color v)]
                    (assoc p :color (color/set-color c))
                    p))
                p)))
          rows)

    :flags
    (let [v2style (or v2style default-flag-styles)]
      (->> rows
           (filter (fn [row] (contains? v2style (get row column))))
           (mapv (fn [row]
                   (let [v (get row column)]
                     {:x     (:epoch row)
                      :title (str v)
                      :shape (get v2style v)
                      :text  (str "col: " column " val: " v)})))))

    :point
    (if vec?
      (->> rows
           (mapcat (fn [row]
                     (let [vs (get row column)]
                       (when (sequential? vs)
                         (map (fn [p] [(:epoch row) p]) vs)))))
           vec)
      (mapv (fn [row] [(:epoch row) (get row column)]) rows))

    :range
    (mapv (fn [row]
            (if (vector? column)
              [(:epoch row) (get row (first column)) (get row (second column))]
              (let [v (get row column)]
                [(:epoch row) (first v) (second v)])))
          rows)

    (mapv (fn [row] [(:epoch row) (get row column)]) rows)))

(defn- series-spec [{:keys [axis column type color opacity mode marker radius
                            on-series fillColor width height] :as _desc}]
  (let [base {:id           (str column)
              :yAxis        axis
              :name         (str column)
              :zIndex       1000
              :animation    false
              :dataGrouping {:enabled false}}]
    (merge base
           (case type
             :flags
             (merge {:type         "flags"
                     :shape        "squarepin"
                     :fillColor    "rgba(255, 255, 255, .4)"
                     :allowOverlapX true
                     :zIndex       9999
                     :onSeries     (name on-series)
                     :onKey        "close"}
                    (cond-> {}
                      fillColor (assoc :fillColor fillColor)
                      width     (assoc :width width)
                      height    (assoc :height height)))

             :step
             {:type  "line"
              :step  true
              :color (color/set-color (or color "blue"))}

             :line
             {:type  "line"
              :color (color/set-color (or color "blue"))}

             :column
             {:type  "column"
              :color (color/set-color (or color "blue"))}

             :range
             {:type        "arearange"
              :color       (color/set-color (or color "blue"))
              :fillOpacity (or opacity 0.75)}

             :ohlc
             {:type (case mode
                      :candle        "candlestick"
                      :candle-hollow "hollowcandlestick"
                      "ohlc")}

             :point
             {:type   "scatter"
              :marker {:enabled   true
                       :symbol    (or marker "circle")
                       :radius    (or radius 2)
                       :fillColor (color/set-color (or color "blue"))}}

             {:type  "line"
              :color (color/set-color (or color "blue"))}))))

(defn- y-axis [n]
  (let [ohlc-height  600
        other-height 100]
    (into []
          (concat
           [(assoc axes-default :top 0 :height ohlc-height)]
           (map (fn [idx]
                  (assoc axes-default
                         :top    (+ ohlc-height (* idx other-height))
                         :height other-height
                         :resize {:enabled (< idx (dec (dec n)))}))
                (range (dec n)))))))

(defn highstock-ds
  "Build a multi-pane Highstock chart from rows and a compact chart spec.

   opts:
     :style   host element CSS (defaults to the computed chart height)
     :class   host element class
     :charts  vector of panes; each pane maps a column to a type spec, e.g.
              [{:close {:type :line}
                :bar   {:type :ohlc :mode :candle}}
               {:volume :column}]

   rows: vector of maps with :epoch (long ms) or :date (Instant / ISO string).
   Supported series types: :line, :step, :column, :range, :ohlc, :point, :flags."
  [{:keys [style class charts] :or {class ""}} rows]
  (let [rows        (mapv ensure-epoch rows)
        descs       (-> charts chart->series add-flags-target)
        series      (mapv (fn [d]
                            (assoc (series-spec d) :data (series-data d rows)))
                          descs)
        n           (count charts)
        chart-height (+ 600 (* 100 (dec n)))
        template    (-> default-template
                        (assoc :yAxis (y-axis n))
                        (assoc-in [:chart :height] chart-height)
                        (assoc :series series))
        style       (merge {:width "100%" :height (str chart-height "px")} style)]
    (highstock (assoc template :style style :class class))))
