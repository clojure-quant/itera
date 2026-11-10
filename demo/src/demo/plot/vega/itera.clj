(ns demo.plot.vega.itera
  (:require [itera.plot.vega :as vega]))

(def spec
  {:$schema "https://vega.github.io/schema/vega/v5.json"
   :width   400
   :height  250
   :padding {:top 10 :left 55 :bottom 40 :right 10}
   :data    [{:name   "table"
              :values [{:x 0 :y 7} {:x 1 :y 8} {:x 2 :y 7}
                       {:x 3 :y 4} {:x 4 :y 6}]}]
   :marks   [{:type   "symbol"
              :from   {:data "table"}
              :encode {:enter  {:x           {:scale "x" :field "x"}
                                :y           {:scale "y" :field "y"}
                                :fill        {:value "steelblue"}
                                :fillOpacity {:value 1}}
                       :update {:shape  "circle"
                                :size   {:value 70}
                                :stroke {:value "transparent"}}
                       :hover  {:size   {:value 210}
                                :stroke {:value "white"}}}}]
   :scales  [{:name   "x" :type "linear" :range "width" :zero false
              :domain {:data "table" :field "x"}}
             {:name   "y" :type "linear" :range "height" :nice true :zero false
              :domain {:data "table" :field "y"}}]
   :axes    [{:orient "bottom" :scale "x"}
             {:orient "left" :scale "y"}]})

(defn calculate [_ctx _opts] spec)

(defn plot [_opts spec]
  (vega/vega {:spec       spec
              :embed-opts {:theme "quartz"}
              :style      {:width "100%" :height "400px"}}))

(def algo-vega
  {:calculate/fn calculate
   :plot/fn      plot})
