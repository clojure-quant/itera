(ns demo.main
  "Demo server entry point.

   Requires every itera.plot namespace so (a) their client components register
   in the Hyper component registry at load time (before the handler assembles
   the JS bundle) and (b) their CSS `head` hiccup is available, then starts the
   itera server with the composed head."
  (:require [itera.hyper :as hyper]
            [itera.plot.aggrid :as aggrid]
            [itera.plot.cheetah :as cheetah]
            [itera.plot.cytoscape :as cytoscape]
            [itera.plot.echarts :as echarts]
            [itera.plot.fipp :as fipp]
            [itera.plot.highcharts :as highcharts]
            [itera.plot.md :as md]
            [itera.plot.pixi-timeseries :as pixi-timeseries]
            [itera.plot.plotly :as plotly]
            [itera.plot.rtable :as rtable]
            [itera.plot.vega :as vega]))

(defn- head
  "Compose the stylesheet head from all viewer namespaces."
  []
  (vec (concat aggrid/head
               cheetah/head
               cytoscape/head
               echarts/head
               fipp/head
               highcharts/head
               md/head
               pixi-timeseries/head
               plotly/head
               rtable/head
               vega/head)))

(defn start
  "Hyper :exec-fn. Receives {:opts {...}} from the :itera alias exec-args."
  [{:keys [opts]}]
  (hyper/start {:ctx  {}
                :opts (merge {:port 5000 :static-resources "public"}
                             opts
                             {:head (head)})}))
