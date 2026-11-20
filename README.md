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

The demo datasets are committed as CSV under `demo/resources/data/`:

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



create itera.plot.rtable-format-ui ns

(defn rtable-format-edit-ui [data opts])

this should be used in itera.plot.rtable - spec-editor

create a multimethod format-rtable-col 

create a multimethod style-rtable-col 

:prct should display a double in prct. 
:nr-format-auto should be similar to: https://github.com/pink-gorilla/dali-table/blob/main/dali-table/src/rtable/format/number.cljs (but in clj)
:format-date should be similar to: https://github.com/pink-gorilla/dali-table/blob/main/dali-table/src/rtable/format/date.cljs but in clj. 

the ui is a table 
columns: - column (can select all columns in the data)
         - width a textbox where numbers can be inputted   
         - format (can select any method that implements multimethod format-rtable-col)  
         - format-opts (a textbox where something can be entered. this will be a parameter passed to format)
         - style (can select any method that implemetns multimethod style-rtable-col)

the rtable default ui shoudl also have padding-sm table-striped table-hover
see: https://github.com/pink-gorilla/dali-table/blob/main/dali-table/src/notebook/dali/rtable/table/rtable.clj 

in the model rtable 
add a new formula to generate persons:
https://github.com/pink-gorilla/dali-table/blob/main/demo/src/demo/table/data2.cljs

and then set the edit spec to have custom styled columns.

the custom style columns need to be defined in (:ns-data model)
which the user can go by going to the tab ns. three you need to add a require
so that the multimethods come to scope. and then you define them similar to here:

https://github.com/pink-gorilla/dali-table/blob/main/demo/src/demo/table/columns2.cljs























