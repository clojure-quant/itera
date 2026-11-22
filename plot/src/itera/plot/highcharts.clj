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

   For row-based multi-pane charts with an editable plot/series spec, see
   itera.plot.highcharts-timeseries.

   The stylesheet is served locally from the plot sub-project's resources."
  (:require [hyper.core :as h]
            [itera.value.protocol]))

(def head
  [[:link {:rel  "stylesheet"
           :href "/itera/plot/highcharts/css/highcharts.css"}]])

(defrecord Highchart [hiccup]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] hiccup))

(defrecord Highstock [hiccup]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] hiccup))

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
  (Highchart.
   (itera-highchart (cond-> {:options (dissoc opts :style :class)}
                      style (assoc :style style)
                      class (assoc :class class)))))

(defn highstock
  [{:keys [style class] :as opts}]
  (Highstock.
   (itera-highstock (cond-> {:options (dissoc opts :style :class)}
                      style (assoc :style style)
                      class (assoc :class class)))))