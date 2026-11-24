# itera [![Clojars: itera](https://img.shields.io/clojars/v/io.github.clojure-quant/itera.svg?label=itera)](https://clojars.org/io.github.clojure-quant/itera) [![Clojars: itera-plot](https://img.shields.io/clojars/v/io.github.clojure-quant/itera-plot.svg?label=itera-plot)](https://clojars.org/io.github.clojure-quant/itera-plot) [![Clojars: itera-pixi-timeseries](https://img.shields.io/clojars/v/io.github.clojure-quant/itera-pixi-timeseries.svg?label=itera-pixi-timeseries)](https://clojars.org/io.github.clojure-quant/itera-pixi-timeseries)
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/backtest.cljs

"A modern Excel Sheet in the Browser. Self Hosted."

- Like in excel, you have cells into which you can enter values or formulas.
  - unlike excel this cells are not organized in a table, but in a graph editor.
- the types in each cell can be complex. So the value cells have special
  (extensible) editors. 
  - The option-ui editor allows to create the structure of a map
    which can hold multiple keys, which can have different types
  - The edn editor allows you to define arbitrary edn structure.
- the inter(ative) view shows input-ui on the left (the one you
  defined when you built the graph) and a document of all cells that
  you want to see.
- cells can be rendered with custom ui functions. 
  - table (which ui to define columns and its formats)
  - timeseries-ui (a special timeseries plot function)
  - vega/vegalite
- the entire graph calculates lazy on demand. 
  changing inputs, causes rerenders. good for data exploration
  or as a data pipeline playground.


## Run the Demo

clone this repo
```
cd demo
clj -X:itera
```
open browser on localhost:5000/itera

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

create itera.plot.template

(defmulti templateable-plot
  (fn [plot-type] plot-type))

(defrecord TemplatePlot [base template data]
  protocol/itera-convertable
  (to-hiccup [_]
    (to-hiccup base data))
  protocol/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (to-formatted-hiccup base data spec window)))
  (default-spec [_]
    (load-from-resources template)
  (spec-editor [_ spec code-a]
    (spec-editor base data spec code-a))))

  protocol/itera-page-plot
  (to-formatted-paged-hiccup [v spec window page]
  (to-formatted-paged-hiccup [base spec window])
  (init-page [_ _spec window]
    (init-page base spec window))
  (page-editor [v spec window page-a]
    (page-editor base spec window page-a)))

(defn draw-template [plot-type template data]
  (let [base (template plot-type)
        template (slurp (str "resources/itera/template/" template ".edn"))]
    (TemplatePlot. base template data)))