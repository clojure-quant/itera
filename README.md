



https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/backtest.cljs



create plot folder as a sub-project

1.from this project: https://github.com/pink-gorilla/dali-table/tree/main/demo/src/demo/page/viewer
implement:
itera.plot.aggrid
itera.plot.cheetah
itera.plot.highcharts/highchart
itera.plot.highcharts/highstock
itera.plot.plotly
itera.plot.rtable
itera.plot.vega/vegalite
itera.plot.vega/vega

this are viewers that were using cljs. we want to reimplement them in hyper.

2. demo project add demo.plot.aggrid/itera (and so forth) , that use
no options, and in calculate just return sample data.
the sample can be eiter in code (if it is a little data), or they can be
loaded from a resource folder in demo. 

this are example pages we could use for that:
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/vega.cljs
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/highchart.cljs
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/rtable.cljs
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/vegads.cljs
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/publish/vega.clj
https://github.com/pink-gorilla/dali-table/blob/main/dali-table/src/rtable/viewer/aggrid.cljs
https://github.com/pink-gorilla/dali-table/blob/main/dali-table/src/rtable/viewer/cheetah.cljs

https://github.com/clojure-quant/quanta-studio/blob/main/src-junk/vega.clj

3. extend options system, to allow :date control
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/date.cljs
is a implemented version


add itera.plot.md
add itera.plot.md/md (show markdown from string)
and itera.plot.md/md-resource (show markdown from resource file)
https://github.com/clojure-quant/quanta-studio/blob/main/src/quanta/dali/plot/md.clj
is an example that uses cljs 

add itera.plot.fipp
this is an example that uses cljs:
https://github.com/clojure-quant/quanta-studio/blob/main/src/quanta/dali/plot/fipp.clj

add examples in: demo.plot.fipp/itera and demo.plot.md/itera