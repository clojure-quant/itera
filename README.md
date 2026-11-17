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


create itera.input.option.

(defn option-input [options-a])

a ui control that is a table with
3 columns
1. name - a string that is edited in a textbox.
   all spaces that are entered into the textbox
   need to be saved before it is stored into :path [:name]
   the value is stored as [:name] so the text is converted
   to a keyword and then stored into a vector.
2. editor
   a combobox of available editors
   itera.option.default-ui defines somes.
   the list needs to come from all implementations of the 
   multimethod
3. spec
   this is a textbox of multiple lines of text.

it allows to add new lines to the list
and it allows to delete an existing line.   

(defn create-option-spec [workbook options])
 this transforms for each row that has :spec as a string
 by using itera.util.clj/compile-formula


create demo.page.option
(defn page-option-playground)
in demo.main this page needs to be added to the hyper router.
itera.hyper/start needs to accept a new :routes parameter (optional)
if set, it will add the routes to the website.

the page displays on the left 
itera.input.option/option-input
intitally with [] in the atom.

on the right it will display move itera.util to itera.uitl.clj

in the upper part
itera.option.core/options-ui.
the :spec will come from the left side.move itera.util to itera.uitl.clj

the displayed value will initially be {} in an atommove itera.util to itera.uitl.clj

in the lower part
it will display (pr-str value)
