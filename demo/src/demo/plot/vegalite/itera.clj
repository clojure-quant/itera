(ns demo.plot.vegalite.itera
  (:require [itera.plot.vega :as vega]))

(def spec
  {:width       500
   :height      300
   :description "NAV Plot"
   :mark        "line"
   :encoding    {:x {:field "index" :type "ordinal"}
                 :y {:field "nav" :type "quantitative"}}})

(def data
  [{:nav 100.0 :index 1}
   {:nav 120.0 :index 2}
   {:nav 150.0 :index 3}
   {:nav 120.0 :index 4}
   {:nav 140.0 :index 5}
   {:nav 150.0 :index 6}
   {:nav 160.0 :index 7}
   {:nav 150.0 :index 8}])

(defn calculate [_ctx _opts] data)

(defn plot [_opts rows]
  (vega/vegalite {:spec       spec
                  :data       rows
                  :embed-opts {:theme "quartz"}
                  :style      {:width "100%" :height "400px"}}))

(def algo-vegalite
  {:calculate/fn calculate
   :plot/fn      plot})
