
https://github.com/clojure-quant/quanta-studio/blob/main/dev-plot/src/quanta/studio/dev/page/viewer/backtest.cljs

1.
https://github.com/pink-gorilla/dali-table/tree/main/dali-table/resources/public

copy aggrid, highcharts, vega, rtable, flags
to plot/resources/itera/plot/

2. in itera project
change "/itera/:ns/:symbol" to 
"/itera/algo/:ns/:symbol"

and add "/itera/public/"
to serve resource path "itera"
this will allow to load all the necessary css files
you copied in step 1

3. ensure that
highcharts, aggrid, vega, rtable load the right
css files. 


