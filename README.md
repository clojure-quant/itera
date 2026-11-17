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



add itera.page.model-overview
similar to itera.page.overview
but list of models comes from itera.runner.ui/available-models

rename itera.page.runner to itera.page.model
and change its route to /itera/model/:model-name

keep all the ui that is currently in there,
but the page structure should be:
 - the page should have width view-port and height view-port
 - on the top is a small header showing "Model: " (and model-name)
   with fixed height of 2 rem (or appropriate)
 - below is a tab element that fills all the remaining space
   - the first tab is "graph"
   - the second tab is "interact"


 content of graph tab:
 inside the graph tab, there is a header on top,
 and the body. 

 this tab is very similar to the itera.page.graph. I will list differences:

 1. available node-types are just :input and :formula
    - both :input and :formula have one output 
    - :input has no inputs. 
      :formula has zero or more outputs. the number and names of outputs
      can change. they are determined by
      taking :args from the result of itera.runner.formula/create-formula
      also look at itera.runner.core and itera.runner.ui to see where this
      data is obtained in the best way.

2. data management happens only via itera.runner.ui (and new functions that
   are put in there). the entire datastructure is manage in itera.runner.ui
   you can write new functions in itera.runner.ui

3. visual appearance of a node:
   - every node has ;show? displayed in the top-right of the node-box.
     I want to see the symbol of an eye when showing, and a shaded out eye
     when not showing. (search for good emojis / unicode characters) 
   - in the left the node-box is the id, which is text and can be edited
   - to the left of the :show? symbol is the edit-symbol box (put a nice 
     emoji / unicode character) A :formula node the :edit click will 
     open a codemirror editor as a popup (see itera.input.codemirror)
     a :input node will open a itera.input.option as a popup. When the 
     popup is closed, the data will be saved to the node. In a formula
     this means that the :out needs to be recreated, as it is 
     done in itera.runner.core/create-formula-node (but instead of 
     adding a new node, the old node needs to be updated)
   - input-output nodes are shown as in itera.page.graph
   - no other information is displayed for a node  

4. - the control that shows all the nodes should have scrollbars, if
     there are nodes that are outside of the visible area. it should 
     have a zoom in/out button that will make all nodes smaller/bigger
     a node that does not have a window-position on loading will be
     put somewhere in the visible area of the node-area.

interact tab:
  - the interact tab should have a sidebar to the left, and
    a main section that fills the rest.   
  - move all the current ui to the main section of the interact tab 
    (other than the header)
  - the sidebar shows multiple option-uis. 
    similar to: itera.page.algo.  add to itera.runner.ui a function 
    that always returns all nodes fo type :input 
    whose option-ui for each node that has a valid option-ui spec
    

      










