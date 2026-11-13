(ns demo.plot.chart.vegalite.itera
  (:require [demo.data :as data]
            [itera.plot.vega :as vega]))

(def nav-spec
  {:width       500
   :height      300
   :description "NAV Plot"
   :mark        "line"
   :encoding    {:x {:field "index" :type "ordinal"}
                 :y {:field "nav" :type "quantitative"}}})

(def nav-rows
  [{:nav 100.0 :index 1}
   {:nav 120.0 :index 2}
   {:nav 150.0 :index 3}
   {:nav 120.0 :index 4}
   {:nav 140.0 :index 5}
   {:nav 150.0 :index 6}
   {:nav 160.0 :index 7}
   {:nav 150.0 :index 8}])

(def point-spec
  {:mark     :point
   :encoding {:x {:field :x :type :quantitative}
              :y {:field :y :type :quantitative}}})

(def point-rows
  [{:x 3 :y 4}
   {:x 7 :y 1}
   {:x 5 :y 7}
   {:x 2 :y 6}])

(def bar-spec
  {:description "A simple bar chart with embedded data."
   :mark        {:type "bar" :tooltip {:content "data"}}
   :encoding    {:x {:field "a" :type "ordinal"}
                 :y {:field "b" :type "quantitative"}}})

(def bar-rows
  [{:a "A" :b 28} {:a "B" :b 55} {:a "C" :b 43} {:a "D" :b 91}
   {:a "E" :b 81} {:a "F" :b 53} {:a "G" :b 19} {:a "H" :b 87}
   {:a "I" :b 52} {:a "J" :b 127}])

(def stocks-spec
  {:description "Stock prices of 5 Tech Companies over Time."
   :mark        "line"
   :encoding    {:x     {:field "date" :type "temporal"}
                 :y     {:field "price" :type "quantitative"}
                 :color {:field "symbol" :type "nominal"}}})

(def examples
  {:vegalite/nav
   {:label "NAV"
    :data  (fn [] {:spec nav-spec :data nav-rows})}

   :vegalite/point
   {:label "Point"
    :data  (fn [] {:spec point-spec :data point-rows})}

   :vegalite/bar
   {:label "Bar"
    :data  (fn [] {:spec bar-spec :data bar-rows})}

   :vegalite/stocks
   {:label "Stocks"
    :data  (fn [] {:spec stocks-spec :data (data/stocks)})}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :vegalite/nav}}]
  (let [{:keys [data]} (get examples example)]
    (assoc (data) :example example)))

(defn plot [_opts {:keys [spec data]}]
  (vega/vegalite {:spec       spec
                  :data       data
                  :embed-opts {:theme "quartz"}
                  :style      {:width "100%" :height "400px"}}))

(def algo-vegalite
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :vegalite/nav}
   :plot/options {}
   :plot/fn plot})
