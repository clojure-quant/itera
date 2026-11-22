(ns itera.plot.highcharts-timeseries-types
  "Series types supported by the highcharts-timeseries viewer.

   This namespace is the single source of truth for the supported series
   types and how many data columns each one consumes.  Both the viewer and
   the spec editor require it; neither keeps its own copy.

   :range / :columnrange use two columns ([low high]); :hlc uses three
   ([high low close]); :ohlc uses four ([open high low close]).  The rest
   use a single column.")

(def types
  "All series types the highcharts-timeseries viewer can render."
  [:line :step :column :spline :area :areaspline
   :columnrange :hlc :range :ohlc :point :flags])

(def column-count
  "Number of data columns each series type consumes."
  {:line        1
   :step        1
   :column      1
   :spline      1
   :area        1
   :areaspline  1
   :columnrange 2
   :range       2
   :hlc         3
   :ohlc        4
   :point       1
   :flags       1})