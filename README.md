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



pixi-timeseries-format-edit-ui
should get a nicer ui.

PANES:

1. "Pane 1 (2 plots)" is the pane header.
 in the very right of the pane header should be a X in red color
 with borders. this will remove a pane. remove the button "Remove Pane"
2. The "+ Pane" button should be after all panes.
3. "+ Plot" button should be below all the plots within a pane
4. the pane should have a xAxisScale option that can be either :linear
   or :log (this needs an adjustment in the plot function)

PLOT
 All Plots should be in a Table with 3 columns:
 - Type
 - Column(s)
 - Opts

 The type column should be 20em wide
 The columns column should be 20 em wide
 The opts column should fill the remaining space

 For type BAR the columns columns will have 4 rows.
  Open: xxx
  High: xxx
  Low: xxx
  Close: xxx
A bar plot does not have a color.

The color picker should have all colors sorted by name


