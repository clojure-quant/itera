(ns demo.main
  "Demo server entry point.

   Plot assets are discovered from the `itera-plots.edn` resources shipped by
   the itera, plot and pixi-timeseries projects (see itera.plot-discover):
   their head namespaces load the plot client components, and their option-UI
   namespaces register the option editors, before the handler assembles the
   JS bundle."
  (:require [demo.graph :as demo-graph]
            [itera.hyper :as hyper]))

(defn start
  "Hyper :exec-fn. Receives {:opts {...}} from the :itera alias exec-args."
  [{:keys [opts]}]
  (hyper/start {:ctx  {:graph {:node-types demo-graph/node-types
                               :initial-graph demo-graph/initial-graph}}
                :opts (merge {:port 5000 :static-resources "public"}
                             opts)}))
