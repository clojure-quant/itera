(ns itera.plot.pixi-timeseries.plot-type
  "Plot types supported by the pixi-timeseries viewer.

   This vector is the single source of truth for plot types used by the
   plot-format-ui spec editor.  It mirrors the viewer's rendering: the client component
   is compiled to browser JS and dispatches on these type strings, so it cannot
   reference this server-side vector directly.

   :bar renders OHLC candlesticks and uses a 4-element :col
   ([open high low close]); the other types use a single column (or a pair
   for :range).  :point-drop uses a scalar :col plus an optional
   :base-value number (default 0) and draws a vertical drop line from each
   point to that base.  :background-color uses a single boolean :col plus
   :color; it paints a pane background band for every row whose value is
   true, contributes nothing to the y scale, and is ignored when the column
   is not boolean.")

(def types
  "All plot types the pixi viewer can render."
  [:line :point :point-drop :range :signal :bar :background-color])
