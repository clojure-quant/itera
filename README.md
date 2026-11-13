
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
shown in the demo of demo.plot.rtable.itera
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










