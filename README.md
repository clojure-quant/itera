# itera [![Clojars: itera](https://img.shields.io/clojars/v/io.github.clojure-quant/itera.svg?label=itera)](https://clojars.org/io.github.clojure-quant/itera) [![Clojars: itera-plot](https://img.shields.io/clojars/v/io.github.clojure-quant/itera-plot.svg?label=itera-plot)](https://clojars.org/io.github.clojure-quant/itera-plot) [![Clojars: itera-pixi-timeseries](https://img.shields.io/clojars/v/io.github.clojure-quant/itera-pixi-timeseries.svg?label=itera-pixi-timeseries)](https://clojars.org/io.github.clojure-quant/itera-pixi-timeseries)
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/backtest.cljs



["/public" 
     {:name :itera-public
      :title "Itera Public"
      :hyper/disabled? true
      :get (fn [req]
             ; serve resource 
                )}]

---

create itera.plot/glide-grid/glide-grid
it is based on npm "@glideapps/glide-data-grid" 
a native javascript if possible. do not add react dependencies
it syntax should be identical to itera.plot.rtable/rtable as
shown in the demo of demo.plot.table.rtable.itera
add a demo for glide-grid. 



## Demo data

The demo datasets are committed as CSV under `demo/resources/public/data/`:

- `bars-1m-full.csv` — 1-minute bars (10k rows)
- `roundtrips.csv` — backtest roundtrips
- `stocks.csv` — daily stock prices

They are loaded at runtime by `demo.data` (cached with `delay`). To regenerate
them from the Dali transit-json sources, run from `demo/`:

    clojure -M:convert

The converter (`demo.convert`) is dev-only; the committed CSVs are what the
demo server reads.



https://github.com/pink-gorilla/reval/blob/master/reval-ui/src/reval/dali/viewer/evalerr.cljs

https://github.com/pink-gorilla/dali/blob/main/src/dali/spec.cljc

 reval.helper.ui-helper


(requiring-resolve '.../cars) returns a Var, which cheshire cannot serialize (Cannot JSON encode object of class: class clojure.lang.Var). I deref it. @ is not EDN and breaks util/fn-arglist (edn/read-string), so I used (deref (requiring-resolve '...)).


itera.plot.pixi-timeseries
add a new plot type :background-color
 the options are :col which can select from any :bool column
 we have an additional argument :color
 which can be selected from all colors from itera.util.color

when a background-color plot is added to a pane,
then it will draw the background of the pane for that x value
in the specified color. it will not have an impact on the 
y scale.
 
create a new model "pixi-timeseries-background"
add a formula cell where you generate dummy data
with 1000 random bars, and then approximately 5% of 
all bars should have :bear-market :true. make sure that
the bear-market is true, that this is not only for 
one row, but for multiple subsequent rows. 



 
