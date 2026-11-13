(ns itera.plot.pixi.plot-type
  "Plot types supported by the pixi-timeseries viewer.

   This vector is the single source of truth for plot types used by the
   :chartspec editor.  It mirrors the viewer's rendering: the client component
   is compiled to browser JS and dispatches on these type strings, so it cannot
   reference this server-side vector directly.

   :bar renders OHLC candlesticks and uses a 4-element :col
   ([open high low close]); the other types use a single column (or a pair
   for :range).")

(def types
  "All plot types the pixi viewer can render."
  [:line :point :range :signal :bar])
