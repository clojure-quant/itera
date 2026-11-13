(ns demo.plot.chart.vega.itera
  (:require [itera.plot.vega :as vega]))

(def symbol-spec
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

(def falkensee-spec
  {:$schema "https://vega.github.io/schema/vega/v5.json"
   :height 350
   :description "An annotated line chart of the population of Falkensee, Germany."
   :legends [{:fill "color"
              :title "Period"
              :orient "top-left"
              :offset 8
              :encode {:symbols {:update {:strokeWidth {:value 2}
                                          :shape {:value "square"}
                                          :opacity {:value 0.3}}}}}]
   :config {:title {:fontSize 16}}
   :axes [{:orient "left" :scale "y" :title "Population" :titlePadding 10 :grid true}
          {:orient "bottom" :scale "x" :format "d" :title "Year" :tickCount 15}]
   :width 500
   :scales [{:name "x" :type "linear" :range "width" :zero false
             :domain {:data "table" :field "year"}}
            {:name "y" :type "linear" :range "height" :nice true :zero true
             :domain {:data "table" :field "population"}}
            {:name "color" :type "ordinal"
             :domain {:data "annotation" :field "mtext"}
             :range ["black" "red"]}]
   :title {:text {:signal "'Population of Falkensee from ' + years[0] + ' to ' + years[1]"}}
   :padding 5
   :marks [{:type "rect"
            :from {:data "annotation"}
            :encode {:enter {:x {:scale "x" :field "start"}
                             :x2 {:scale "x" :field "end"}
                             :y {:value 0}
                             :y2 {:signal "height"}
                             :fill {:scale "color" :field "mtext"}
                             :opacity {:value 0.2}}}}
           {:type "line"
            :from {:data "table"}
            :encode {:enter {:interpolate {:value "monotone"}
                             :x {:scale "x" :field "year"}
                             :y {:scale "y" :field "population"}
                             :stroke {:value "steelblue"}
                             :strokeWidth {:value 3}}}}
           {:type "symbol"
            :from {:data "table"}
            :encode {:enter {:x {:scale "x" :field "year"}
                             :y {:scale "y" :field "population"}
                             :stroke {:value "steelblue"}
                             :strokeWidth {:value 1.5}
                             :fill {:value "white"}
                             :size {:value 30}}}}]
   :data [{:name "table"
           :values [{:year 1875 :population 1309}
                    {:year 1890 :population 1558}
                    {:year 1910 :population 4512}
                    {:year 1925 :population 8180}
                    {:year 1933 :population 15915}
                    {:year 1939 :population 24824}
                    {:year 1946 :population 28275}
                    {:year 1950 :population 29189}
                    {:year 1964 :population 29881}
                    {:year 1971 :population 26007}
                    {:year 1981 :population 24029}
                    {:year 1985 :population 23340}
                    {:year 1989 :population 22307}
                    {:year 1990 :population 22087}
                    {:year 1991 :population 22139}
                    {:year 1992 :population 22105}
                    {:year 1993 :population 22242}
                    {:year 1994 :population 22801}
                    {:year 1995 :population 24273}
                    {:year 1996 :population 25640}
                    {:year 1997 :population 27393}
                    {:year 1998 :population 29505}
                    {:year 1999 :population 32124}
                    {:year 2000 :population 33791}
                    {:year 2001 :population 35297}
                    {:year 2002 :population 36179}
                    {:year 2003 :population 36829}
                    {:year 2004 :population 37493}
                    {:year 2005 :population 38376}
                    {:year 2006 :population 39008}
                    {:year 2007 :population 39366}
                    {:year 2008 :population 39821}
                    {:year 2009 :population 40179}
                    {:year 2010 :population 40511}
                    {:year 2011 :population 40465}
                    {:year 2012 :population 40905}
                    {:year 2013 :population 41258}
                    {:year 2014 :population 41777}]
           :transform [{:type "extent" :field "year" :signal "years"}]}
          {:name "annotation"
           :values [{:start 1933 :end 1945 :mtext "Nazi Rule"}
                    {:start 1948 :end 1989 :mtext "GDR (East Germany)"}]}]})

(def rect-spec
  {:$schema "https://vega.github.io/schema/vega/v5.json"
   :width 400
   :height 400
   :signals [{:name "tooltip"
              :value {}
              :on [{:events "rect:mouseover" :update "datum"}
                   {:events "rect:mouseout" :update "{}"}]}]
   :scales [{:name "xscale"
             :type "band"
             :domain {:data "table" :field "category"}
             :range "width"}
            {:name "yscale"
             :domain {:data "table" :field "amount"}
             :nice true
             :range "height"}]
   :axes [{:orient "bottom" :scale "xscale"}
          {:orient "left" :scale "yscale"}]
   :marks [{:type "rect"
            :from {:data "table"}
            :encode {:enter {:x {:scale "xscale" :field "category" :offset 1}
                             :width {:scale "xscale" :band 1 :offset -1}
                             :y {:scale "yscale" :field "amount"}
                             :y2 {:scale "yscale" :value 0}}
                     :update {:fill {:value "steelblue"}}
                     :hover {:fill {:value "red"}}}}
           {:type "text"
            :encode {:enter {:align {:value "center"}
                             :baseline {:value "bottom"}
                             :fill {:value "#333"}}
                     :update {:x {:scale "xscale" :signal "tooltip.category" :band 0.5}
                              :y {:scale "yscale" :signal "tooltip.amount" :offset -2}
                              :text {:signal "tooltip.amount"}
                              :fillOpacity [{:test "datum === tooltip" :value 0}
                                            {:value 1}]}}}]})

(def rect-rows
  [{:category "A" :amount 28}
   {:category "B" :amount 55}
   {:category "C" :amount 43}
   {:category "D" :amount 91}
   {:category "E" :amount 81}
   {:category "F" :amount 53}
   {:category "G" :amount 19}
   {:category "H" :amount 87}])

(defn- zoom-points []
  (mapv (fn [i]
          {:u (* 10 (Math/sin (* i 0.1)))
           :v (* 10 (Math/cos (* i 0.13)))})
        (range 100)))

(def zoom-spec
  {:$schema "https://vega.github.io/schema/vega/v5.json"
   :description "An interactive scatter plot example supporting pan and zoom."
   :autosize "none"
   :width 500
   :height 300
   :config {:axis {:domain false
                   :tickSize 3
                   :tickColor "#888"
                   :labelFont "Monaco, Courier New"}}
   :axes [{:scale "xscale" :orient "top" :offset {:signal "xoffset"}}
          {:scale "yscale" :orient "right" :offset {:signal "yoffset"}}]
   :scales [{:name "xscale"
             :zero false
             :domain {:signal "xdom"}
             :range {:signal "xrange"}}
            {:name "yscale"
             :zero false
             :domain {:signal "ydom"}
             :range {:signal "yrange"}}]
   :marks [{:type "symbol"
            :from {:data "points"}
            :clip true
            :encode {:enter {:fillOpacity {:value 0.6} :fill {:value "steelblue"}}
                     :update {:x {:scale "xscale" :field "u"}
                              :y {:scale "yscale" :field "v"}
                              :size {:signal "size"}}
                     :hover {:fill {:value "firebrick"}}
                     :leave {:fill {:value "steelblue"}}
                     :select {:size {:signal "size" :mult 5}}
                     :release {:size {:signal "size"}}}}]
   :signals [{:name "margin" :value 20}
             {:name "hover" :on [{:events "*:mouseover" :encode "hover"}
                                 {:events "*:mouseout" :encode "leave"}
                                 {:events "*:mousedown" :encode "select"}
                                 {:events "*:mouseup" :encode "release"}]}
             {:name "xoffset" :update "-(height + padding.bottom)"}
             {:name "yoffset" :update "-(width + padding.left)"}
             {:name "xrange" :update "[0, width]"}
             {:name "yrange" :update "[height, 0]"}
             {:name "down" :value nil :on [{:events "touchend" :update "null"}
                                           {:events "mousedown, touchstart" :update "xy()"}]}
             {:name "xcur" :value nil :on [{:events "mousedown, touchstart, touchend"
                                            :update "slice(xdom)"}]}
             {:name "ycur" :value nil :on [{:events "mousedown, touchstart, touchend"
                                            :update "slice(ydom)"}]}
             {:name "delta" :value [0 0]
              :on [{:events [{:source "window"
                              :type "mousemove"
                              :consume true
                              :between [{:type "mousedown"} {:source "window" :type "mouseup"}]}
                             {:type "touchmove"
                              :consume true
                              :filter "event.touches.length === 1"}]
                    :update "down ? [down[0]-x(), y()-down[1]] : [0,0]"}]}
             {:name "anchor" :value [0 0]
              :on [{:events "wheel"
                    :update "[invert('xscale', x()), invert('yscale', y())]"}
                   {:events {:type "touchstart" :filter "event.touches.length===2"}
                    :update "[(xdom[0] + xdom[1]) / 2, (ydom[0] + ydom[1]) / 2]"}]}
             {:name "zoom" :value 1
              :on [{:events "wheel!"
                    :force true
                    :update "pow(1.001, event.deltaY * pow(16, event.deltaMode))"}
                   {:events {:signal "dist2"} :force true :update "dist1 / dist2"}]}
             {:name "dist1" :value 0
              :on [{:events {:type "touchstart" :filter "event.touches.length===2"}
                    :update "pinchDistance(event)"}
                   {:events {:signal "dist2"} :update "dist2"}]}
             {:name "dist2" :value 0
              :on [{:events {:type "touchmove"
                             :consume true
                             :filter "event.touches.length===2"}
                    :update "pinchDistance(event)"}]}
             {:name "xdom" :update "slice(xext)"
              :on [{:events {:signal "delta"}
                    :update "[xcur[0] + span(xcur) * delta[0] / width, xcur[1] + span(xcur) * delta[0] / width]"}
                   {:events {:signal "zoom"}
                    :update "[anchor[0] + (xdom[0] - anchor[0]) * zoom, anchor[0] + (xdom[1] - anchor[0]) * zoom]"}]}
             {:name "ydom" :update "slice(yext)"
              :on [{:events {:signal "delta"}
                    :update "[ycur[0] + span(ycur) * delta[1] / height, ycur[1] + span(ycur) * delta[1] / height]"}
                   {:events {:signal "zoom"}
                    :update "[anchor[1] + (ydom[0] - anchor[1]) * zoom, anchor[1] + (ydom[1] - anchor[1]) * zoom]"}]}
             {:name "size" :update "clamp(20 / span(xdom), 1, 1000)"}]
   :data [{:name "points"
           :values (zoom-points)
           :transform [{:type "extent" :field "u" :signal "xext"}
                       {:type "extent" :field "v" :signal "yext"}]}]})

(def examples
  {:vega/symbol
   {:label "Symbol"
    :data  (fn [] {:spec symbol-spec})}

   :vega/falkensee
   {:label "Falkensee"
    :data  (fn [] {:spec falkensee-spec})}

   :vega/rect
   {:label "Rect"
    :data  (fn [] {:spec (assoc rect-spec :data [{:name "table" :values rect-rows}])})}

   :vega/zoom
   {:label "Zoom"
    :data  (fn [] {:spec zoom-spec})}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :vega/symbol}}]
  (let [{:keys [data]} (get examples example)]
    (assoc (data) :example example)))

(defn plot [_opts {:keys [spec data]}]
  (vega/vega {:spec       spec
              :data       data
              :embed-opts {:theme "quartz"}
              :style      {:width "100%" :height "400px"}}))

(def algo-vega
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :vega/symbol}
   :plot/options {}
   :plot/fn plot})
