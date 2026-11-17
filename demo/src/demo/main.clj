(ns demo.main
  "Demo server entry point.

   Plot assets are discovered from the `itera-plots.edn` resources shipped by
   the itera, plot and pixi-timeseries projects (see itera.plot-discover):
   their head namespaces load the plot client components, and their option-UI
   namespaces register the option editors, before the handler assembles the
   JS bundle."
  (:require [clojure.string :as str]
            [demo.graph :as demo-graph]
            [demo.page.codemirror :as page-codemirror]
            [demo.page.option :as page-option]
            [demo.page.overview :as page-overview]
            [itera.hyper :as hyper]))

(defn- demo-page-links
  "Extract {:path :name :title} for every route under /demo/ (excluding /demo
   itself) from a reitit route vector."
  [routes]
  (for [[path m] routes
        :when (and (string? path)
                   (str/starts-with? path "/demo/")
                   (:name m))]
    {:path path :name (:name m) :title (:title m)}))

(defn start
  "Hyper :exec-fn. Receives {:opts {...}} from the :itera alias exec-args."
  [{:keys [opts]}]
  (let [ctx {:graph {:node-types demo-graph/node-types
                     :initial-graph demo-graph/initial-graph}
             :itera/model-path "../models"
             :requires '[[clojure.string :as string]
                         [clojure.edn :as edn]
                         [demo.flow :as df]]}
        demo-routes [["/demo/options" {:name :demo-options
                                       :title "Options Playground"
                                       :get (page-option/page-option-playground ctx)}]
                     ["/demo/codemirror" {:name :demo-codemirror
                                          :title "CodeMirror Demo"
                                          :get (page-codemirror/page-codemirror ctx)}]]]
    (hyper/start
     {:ctx ctx
      :routes (into [["/demo" {:name :demo
                               :title "Demo"
                               :get (page-overview/demo-overview-page
                                     (demo-page-links demo-routes))}]]
                    demo-routes)
      :opts (merge {:port 5000 :static-resources "public"}
                   opts)})))
