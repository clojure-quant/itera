(ns demo.main
  "Demo server entry point.

   Plot assets are discovered from the `itera-plots.edn` resources shipped by
   the itera, plot and pixi-timeseries projects (see itera.plot-discover):
   their head namespaces load the plot client components, and their option-UI
   namespaces register the option editors, before the handler assembles the
   JS bundle."
  (:require [demo.graph :as demo-graph]
            [demo.page.option :as page-option]
            [itera.hyper :as hyper]))

(defn start
  "Hyper :exec-fn. Receives {:opts {...}} from the :itera alias exec-args."
  [{:keys [opts]}]
  (let [ctx {:graph {:node-types demo-graph/node-types
                     :initial-graph demo-graph/initial-graph}
             :requires '[[clojure.string :as string]
                         [clojure.edn :as edn]
                         [demo.flow :as df]]}]
    (hyper/start
     {:ctx ctx
      :routes [["/demo/options" {:name :demo-options
                                 :title "Options Playground"
                                 :get (page-option/page-option-playground ctx)}]]
      :opts (merge {:port 5000 :static-resources "public"}
                   opts)})))
