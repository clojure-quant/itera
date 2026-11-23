(ns itera.plot.pixi-timeseries.plot
  "pixi.js time-series viewer as a Hyper client component.

   opts:
     :data    rows (vector of maps) or a column map; required
     :charts  vector of panes, each a vector of series maps:
              {:type one of itera.plot.pixi-timeseries.plot-type/types
               :col  <kw> or [<kw> <kw>] for :range
                     or [<open> <high> <low> <close>] for :bar
               :color \"blue-5\" (name in itera.util.color/colors or a CSS color)}
     :window  plot size, a map:
              {:mode :container|:fixed :width <px> :height <px>}
              :width and :height must be positive integers in both modes;
              pixi never measures the DOM.  With :mode :container the
              dimensions are supplied by itera.container's measurement loop,
              so the caller/page layer must withhold the viewer until that
              loop has measured a complete window.  Absent or invalid
              dimensions render \"invalid :width and :height\" and draw
              nothing.
     :window-selector  window metadata and state, a map:
              {:display :nice|:all
               :step <pixels per row>
               :end <exclusive end row>
               :rows-all <total result rows>
               :rows-visible <rows per page>}
              When present, rows are windowed server-side and the client
              receives only the visible slice; the default view is the last
              page.  :display defaults to :nice (windowed); with :all every
              row is sent and nothing is windowed (:step has no effect there).
              :step (default 8) is server-side only: in :nice mode the rows per
              viewport are derived from the live :window :width and :step on
              every render, so a container resize re-windows the slice; the
              stored :rows-visible is kept for display only and used as a
              fallback when :width is missing.  The plot-page-editor writes
              :end, :display and :step, while `init-window-selector!` fills
              :rows-all and :rows-visible from the result and the window
              width.  Without this key all rows are sent to the client.
     :style   host element CSS, e.g. {:width \"100%\" :height \"800px\"}
     :class   host element class

   Named colors are resolved to hex server-side with itera.util.color before
   the payload reaches the browser: the component below is compiled to
   client-side JS and cannot call the JVM color namespace itself.

   `pixi-timeseries` returns a value that stores only its rows; the charts
   spec and the size are supplied by the itera.value.protocol methods.  The
   editable charts spec is the bare panes vector above, the protocol `window`
   supplies the size, and the page layer supplies the server-side row window
   (navigated by `plot-page-editor`).  The generic `to-hiccup` path uses the
   smart default spec at a fixed 900x600 fallback window because pixi cannot
   measure the DOM.

   `pixi-timeseries-ds` accepts a dataset-like value (tech.ml dataset when
   available, otherwise rows or a column map) and delegates to the rows-only
   constructor.  Column maps whose columns have different lengths are
   truncated to the shortest column.

   `select-rows` and `init-window-selector!` are used internally by the paged
   methods and stay public for the legacy demo."
  (:require [hyper.core :as h]
            [itera.plot.pixi-timeseries.plot-format-ui :as fmt]
            [itera.plot.pixi-timeseries.plot-page-editor :as page-editor]
            [itera.plot.pixi-timeseries.window :as plot-window]
            [itera.util.color :as colors]
            [itera.value.protocol :as protocol]))

(def head [])

(defn- column-map->rows [m]
  (let [ks   (vec (keys m))
        cols (mapv #(vec (get m %)) ks)
        n    (apply min (map count cols))]
    (mapv (fn [i] (zipmap ks (map #(nth % i) cols))) (range n))))

(defn- tech-rows [ds]
  (when-let [rows-fn (try (requiring-resolve 'tech.v3.dataset/rows)
                          (catch Throwable _ nil))]
    (try (vec (rows-fn ds)) (catch Throwable _ nil))))

(defn- ds->rows [ds]
  (or (tech-rows ds)
      (cond
        (nil? ds)        []
        (sequential? ds) (vec ds)
        (map? ds)        (if (every? sequential? (vals ds))
                           (column-map->rows ds)
                           [ds])
        :else            (throw (ex-info "Unsupported dataset value"
                                         {:type (type ds)})))))

(defn- visible-rows
  "Rows per viewport derived from the live :window :width and :step, clamped
   to [1 row-count] (0 for an empty result).  A missing/invalid width falls
   back to row-count."
  [opts row-count]
  (if (pos? row-count)
    (if-let [w (plot-window/rows-visible (:window opts) (:window-selector opts))]
      (max 1 (min row-count w))
      row-count)
    0))

(defn select-rows
  "Return the visible server-side row slice for `opts`.

   `opts` is a plain (dereferenced) plot-options map; window metadata comes
   from :window-selector {:display :nice|:all, :step <pixels per row>,
   :end <exclusive end row>, :rows-all <total rows>, :rows-visible <rows per
   page>}.  :display defaults to :nice; :all returns every row with no
   windowing.  In :nice mode the row count per viewport is derived from the
   live :window :width and :step on every call (so a container resize
   re-windows the slice), falling back to the stored :rows-visible and then
   the row count only when :width is missing/invalid; :end defaults to the
   row count (the last page).  Both are clamped against the rows actually
   passed in, not :rows-all.

   Without a :window-selector, or for non-sequential/empty rows, all rows are
   returned unchanged.

   Do not use this directly as :plot/select: that hook receives the plot
   options cursor, not a map.  Use `init-window-selector!` there."
  [opts rows]
  (let [sel (:window-selector opts)]
    (if (or (nil? sel) (not (sequential? rows)) (empty? rows))
      rows
      (let [rows    (vec rows)
            n       (count rows)
            sel     (if (map? sel) sel {:end sel})
            display (or (:display sel) :nice)]
        (if (= display :all)
          rows
          (let [derived (plot-window/rows-visible (:window opts) sel)
                stored  (:rows-visible sel)
                visible (max 1 (min n (or derived
                                          (when (and (number? stored) (pos? stored))
                                            (long stored))
                                          (visible-rows opts n))))
                end     (let [e (:end sel)]
                          (if (number? e) (long e) n))
                end     (max visible (min n end))]
            (subvec rows (- end visible) end)))))))

(defn init-window-selector!
  "Cursor-aware :plot/select hook for server-side row windowing.

   Receives the plot-options *cursor* plus the algo result (this is why it,
   not `select-rows`, is the :plot/select function).  It derives :rows-all
   from the result, :rows-visible from :window :width / :step, and a clamped
   :end defaulting to the last page, then writes those into the cursor's
   :window-selector while preserving any other selector keys.  The write only
   happens when the selector value actually changes, so the extra render it
   can trigger (first metadata fill, container resize, :step change)
   converges instead of looping.

   Returns the result unchanged."
  [plot-options-a result]
  (let [opts      @plot-options-a
        row-count (if (nil? result) 0 (count result))
        visible   (visible-rows opts row-count)
        current   (:window-selector opts)
        sel       (cond
                    (map? current)    current
                    (number? current) {:end current}
                    :else             {})
        end       (let [e (:end sel)]
                    (if (number? e) (long e) row-count))
        end       (if (pos? row-count)
                    (max visible (min row-count end))
                    0)
        next-sel  (assoc sel
                         :end end
                         :rows-all row-count
                         :rows-visible visible)]
    (when (not= current next-sel)
      (swap! plot-options-a assoc :window-selector next-sel))
    result))

(h/defc itera-pixi-timeseries
  "pixi.js time-series viewer in a custom element. `options` is the
   JSON-parsed viewer payload (`:data`, `:charts`, `:window`); :data holds
   only the server-side visible row slice.  Each pane draws a left-side
   y-axis whose ticks are auto-formatted from that pane's combined series
   bounds; the plot area shrinks to make room for the labels.  The first
   pane also draws a bottom date x-axis with adaptive date/datetime tick
   labels taken from the rows' `\"date\"` column, when present."
  {:require [["https://esm.sh/pixi.js@8.6.6?bundle" :as pixi]]}
  [{:keys [options]}]
  (render
   [:div {:style "display:flex;flex-direction:column;width:100%;height:100%;overflow:hidden"}
    [:div {:class "pixi-canvas" :style "flex:1;min-height:0;position:relative;overflow:hidden"}]
    [:div {:class "pixi-error"
           :style "display:none;flex:1;min-height:0;align-items:center;justify-content:center;color:#b91c1c;font-size:0.85rem"}
     "invalid :width and :height"]])
  (mount [root]
         (let [el (.querySelector root ".pixi-canvas")
               err (.querySelector root ".pixi-error")]
           (set! (.-el ctx) el)
           (set! (.-err ctx) err)
           (set! (.-options ctx) options)
           (set! (.-gen ctx) 0)
           (let [init!
                 (fn []
                   (let [opts   (.-options ctx)
                         el     (.-el ctx)
                         err    (.-err ctx)
                         win    (aget opts "window")
                         ww     (if win (aget win "width") nil)
                         wh     (if win (aget win "height") nil)
                         valid? (and (number? ww) (js/isFinite ww) (> ww 0)
                                     (= ww (js/Math.floor ww))
                                     (number? wh) (js/isFinite wh) (> wh 0)
                                     (= wh (js/Math.floor wh)))]
                     (if (not valid?)
                       (do
                         (set! (.-innerHTML el) "")
                         (set! (.-display (.-style el)) "none")
                         (set! (.-display (.-style err)) "flex")
                         (set! (.-app ctx) nil))
                       (let [w      ww
                             h      (max 1 wh)
                             dpr    (or (.-devicePixelRatio js/window) 1)
                             res    (js/Math.min 2 (js/Math.max 1 dpr))
                             app    (new pixi/Application)
                             g      (inc (or (.-gen ctx) 0))]
                         (set! (.-gen ctx) g)
                         (set! (.-app ctx) app)
                         (set! (.-display (.-style err)) "none")
                         (set! (.-display (.-style el)) "")
                         (set! (.-innerHTML el) "")
                         (.then (.init app (js-obj "width" w "height" h
                                                   "background" "#1099bb"
                                                   "resolution" res
                                                   "autoDensity" true
                                                   "antialias" true))
                                (fn [_]
                                  (when (= g (.-gen ctx))
                                    (let [data      (or (aget opts "data") (array))
                                          charts    (or (aget opts "charts") (array))
                                          stage     (.-stage app)
                                          container (new pixi/Container)
                                          state     (js-obj "width" w "height" h
                                                            "data" data "charts" charts
                                                            "container" container)
                                          true-min (fn [rows col]
                                                     (let [n (.-length rows)]
                                                       (loop [i 0 m js/Infinity]
                                                         (if (< i n)
                                                           (let [v (aget (aget rows i) col)]
                                                             (recur (inc i)
                                                                    (if (and (number? v)
                                                                             (not (js/isNaN v)))
                                                                      (js/Math.min m v)
                                                                      m)))
                                                           m))))
                                          true-max (fn [rows col]
                                                     (let [n (.-length rows)]
                                                       (loop [i 0 m (- js/Infinity)]
                                                         (if (< i n)
                                                           (let [v (aget (aget rows i) col)]
                                                             (recur (inc i)
                                                                    (if (and (number? v)
                                                                             (not (js/isNaN v)))
                                                                      (js/Math.max m v)
                                                                      m)))
                                                           m))))
                                          get-x (fn [geom i]
                                                  ((aget geom "sx") i))
                                          get-y (fn [geom mn mx rows col i]
                                                  ((aget geom "sy") (aget (aget rows i) col) mn mx))
                                          plot-cols (fn [series]
                                                      (let [t (aget series "type")
                                                            c (aget series "col")]
                                                        (cond
                                                          (= t "bar")
                                                          (let [c4 (if (and (array? c)
                                                                            (>= (.-length c) 4))
                                                                     c
                                                                     (array "open" "high"
                                                                            "low" "close"))]
                                                            (array (aget c4 1) (aget c4 2)))

                                                          (= t "range")
                                                          (if (array? c)
                                                            (array (aget c 0) (aget c 1))
                                                            (array))

                                                          (or (= t "line") (= t "point"))
                                                          (array c)

                                                          :else (array))))
                                          bounds-of (fn [rows series]
                                                      (let [cols (plot-cols series)
                                                            nc   (.-length cols)]
                                                        (loop [j 0 mn js/Infinity mx (- js/Infinity)]
                                                          (if (< j nc)
                                                            (let [col (aget cols j)]
                                                              (recur (inc j)
                                                                     (js/Math.min mn (true-min rows col))
                                                                     (js/Math.max mx (true-max rows col))))
                                                            (array mn mx)))))
                                          nice-step (fn [span intervals]
                                                      (let [raw (/ span (max 1 intervals))]
                                                        (if (and (number? raw) (js/isFinite raw)
                                                                 (> raw 0))
                                                          (let [mag (js/Math.pow
                                                                     10 (js/Math.floor
                                                                         (js/Math.log10 raw)))
                                                                n   (/ raw mag)
                                                                m   (cond
                                                                      (<= n 1.5) 1
                                                                      (<= n 3)   2
                                                                      (<= n 7)   5
                                                                      :else      10)]
                                                            (* mag m))
                                                          1)))
                                          tick-values (fn [mn mx ch]
                                                        (if (= mn mx)
                                                          (array (array mn 0))
                                                          (let [intervals (max 1 (min 4
                                                                                      (js/Math.floor (/ ch 35))))
                                                                step      (nice-step (- mx mn) intervals)]
                                                            (if (and (number? step) (js/isFinite step)
                                                                     (> step 0))
                                                              (let [eps   (* step 1e-9)
                                                                    start (* (js/Math.ceil (/ mn step)) step)]
                                                                (loop [v start acc (array)]
                                                                  (if (<= v (+ mx eps))
                                                                    (do
                                                                      (.push acc (array v step))
                                                                      (recur (+ v step) acc))
                                                                    (if (zero? (.-length acc))
                                                                      (array (array (/ (+ mn mx) 2) step))
                                                                      acc))))
                                                              (array (array mn 0))))))
                                          format-tick (fn [v step]
                                                        (let [decimals (if (and (number? step)
                                                                                (> step 0))
                                                                         (max 0 (min 6
                                                                                     (js/Math.ceil
                                                                                      (- (js/Math.log10 step)))))
                                                                         0)
                                                              s        (.toFixed v decimals)]
                                                          (if (pos? decimals)
                                                            (let [t (loop [t s]
                                                                      (if (and (> (.-length t) 1)
                                                                               (.endsWith t "0")
                                                                               (.includes t "."))
                                                                        (recur (.slice t 0 -1))
                                                                        t))
                                                                  t (if (.endsWith t ".")
                                                                      (.slice t 0 -1)
                                                                      t)]
                                                              (if (= t "-0") "0" t))
                                                            s)))
                                          label-font (fn []
                                                       (array "system-ui" "-apple-system" "Segoe UI" "Roboto"
                                                              "Helvetica Neue" "Arial" "sans-serif"))
                                          make-text (fn [text size fill]
                                                      (new pixi/Text
                                                           (js-obj "text" text
                                                                   "roundPixels" true
                                                                   "style" (js-obj "fontFamily" (label-font)
                                                                                   "fontSize" size
                                                                                   "padding" 2
                                                                                   "fill" fill))))
                                          make-labels (fn [ticks]
                                                        (let [n      (.-length ticks)
                                                              labels (array)]
                                                          (loop [i 0]
                                                            (when (< i n)
                                                              (let [tick (aget ticks i)
                                                                    text (make-text
                                                                          (format-tick
                                                                           (aget tick 0)
                                                                           (aget tick 1))
                                                                          11 "#374151")]
                                                                (.set (.-anchor text) 1 0.5)
                                                                (.push labels text))
                                                              (recur (inc i))))
                                                          labels))
                                          add-y-axis (fn [pane geom mn mx ticks labels]
                                                       (when (and (js/isFinite mn) (js/isFinite mx))
                                                         (let [axis-x (aget geom "axis-x")
                                                               y0     (aget geom "y0")
                                                               ch     (aget geom "ch")
                                                               sy     (aget geom "sy")
                                                               n      (.-length ticks)
                                                               line   (new pixi/Graphics)]
                                                           (.moveTo line axis-x y0)
                                                           (.lineTo line axis-x (+ y0 ch))
                                                           (.stroke line (js-obj "width" 1
                                                                                 "color" "#9ca3af"))
                                                           (.addChild pane line)
                                                           (when (pos? n)
                                                             (let [marks (new pixi/Graphics)]
                                                               (loop [i 0]
                                                                 (when (< i n)
                                                                   (let [y (sy (aget (aget ticks i) 0)
                                                                               mn mx)]
                                                                     (when (not (nil? y))
                                                                       (.moveTo marks (- axis-x 3) y)
                                                                       (.lineTo marks axis-x y)))
                                                                   (recur (inc i))))
                                                               (.stroke marks (js-obj "width" 1
                                                                                      "color" "#9ca3af"))
                                                               (.addChild pane marks)))
                                                           (loop [i 0]
                                                             (when (< i n)
                                                               (let [y     (sy (aget (aget ticks i) 0)
                                                                               mn mx)
                                                                     label (aget labels i)]
                                                                 (when (not (nil? y))
                                                                   (set! (.-x label) (js/Math.round (- axis-x 6)))
                                                                   (set! (.-y label) (js/Math.round y))
                                                                   (.addChild pane label)))
                                                               (recur (inc i)))))))
                                          pad2 (fn [n]
                                                 (let [s (str n)]
                                                   (if (< (.-length s) 2)
                                                     (str "0" s)
                                                     s)))
                                          date-parts (fn [v]
                                                       (cond
                                                         (number? v)
                                                         (let [d (new js/Date v)]
                                                           (if (not (js/isFinite (.getTime d)))
                                                             nil
                                                             (array (.getUTCFullYear d)
                                                                    (inc (.getUTCMonth d))
                                                                    (.getUTCDate d)
                                                                    (.getUTCHours d)
                                                                    (.getUTCMinutes d))))

                                                         (string? v)
                                                         (let [n    (.-length v)
                                                               iso? (and (>= n 10)
                                                                         (= (.charAt v 4) "-")
                                                                         (= (.charAt v 7) "-"))]
                                                           (if iso?
                                                             (let [year   (js/parseInt (.slice v 0 4) 10)
                                                                   month  (js/parseInt (.slice v 5 7) 10)
                                                                   day    (js/parseInt (.slice v 8 10) 10)
                                                                   sep    (if (>= n 11)
                                                                            (.charAt v 10)
                                                                            "")
                                                                   hm?    (and (>= n 16)
                                                                               (or (= sep "T") (= sep " "))
                                                                               (= (.charAt v 13) ":")
                                                                               (js/isFinite
                                                                                (js/parseInt (.slice v 11 13) 10))
                                                                               (js/isFinite
                                                                                (js/parseInt (.slice v 14 16) 10)))
                                                                   hour   (if hm?
                                                                            (js/parseInt (.slice v 11 13) 10)
                                                                            0)
                                                                   minute (if hm?
                                                                            (js/parseInt (.slice v 14 16) 10)
                                                                            0)]
                                                               (if (and (js/isFinite year)
                                                                        (>= month 1) (<= month 12)
                                                                        (>= day 1) (<= day 31)
                                                                        (>= hour 0) (<= hour 23)
                                                                        (>= minute 0) (<= minute 59))
                                                                 (array year month day hour minute)
                                                                 nil))
                                                             (let [d (new js/Date v)]
                                                               (if (not (js/isFinite (.getTime d)))
                                                                 nil
                                                                 (array (.getUTCFullYear d)
                                                                        (inc (.getUTCMonth d))
                                                                        (.getUTCDate d)
                                                                        (.getUTCHours d)
                                                                        (.getUTCMinutes d))))))

                                                         :else nil))
                                          has-time? (fn [parts]
                                                      (and (not (nil? parts))
                                                           (or (> (aget parts 3) 0)
                                                               (> (aget parts 4) 0))))
                                          x-label-style (fn [a b]
                                                          (cond
                                                            (or (nil? a) (nil? b)) "ymd"

                                                            (or (has-time? a) (has-time? b))
                                                            (if (and (= (aget a 0) (aget b 0))
                                                                     (= (aget a 1) (aget b 1))
                                                                     (= (aget a 2) (aget b 2)))
                                                              "hm"
                                                              (if (= (aget a 0) (aget b 0))
                                                                "md-hm"
                                                                "ymd-hm"))

                                                            (= (aget a 0) (aget b 0)) "md"
                                                            :else "ymd"))
                                          format-x-label (fn [parts style]
                                                           (let [y  (aget parts 0)
                                                                 mo (pad2 (aget parts 1))
                                                                 d  (pad2 (aget parts 2))
                                                                 h  (pad2 (aget parts 3))
                                                                 mi (pad2 (aget parts 4))]
                                                             (cond
                                                               (= style "md")     (str mo "-" d)
                                                               (= style "ymd")    (str y "-" mo "-" d)
                                                               (= style "hm")     (str h ":" mi)
                                                               (= style "md-hm")  (str mo "-" d " " h ":" mi)
                                                               (= style "ymd-hm") (str y "-" mo "-" d " " h ":" mi)
                                                               :else              (str y "-" mo "-" d))))
                                          tick-indices (fn [n count]
                                                         (let [c    (max 2 (min n count))
                                                               step (/ (dec n) (dec c))]
                                                           (loop [k 0 acc (array)]
                                                             (if (< k c)
                                                               (let [idx (js/Math.round (* k step))]
                                                                 (when (or (zero? (.-length acc))
                                                                           (not= idx (aget acc
                                                                                           (dec (.-length acc)))))
                                                                   (.push acc idx))
                                                                 (recur (inc k) acc))
                                                               acc))))
                                          x-tick-indices (fn [n cw]
                                                           (if (<= n 1)
                                                             (array 0)
                                                             (tick-indices n (max 2 (min 8 n
                                                                                         (js/Math.floor
                                                                                          (/ cw 130)))))))
                                          make-x-labels (fn [ticks style]
                                                          (let [labels (array)
                                                                kept   (array)]
                                                            (loop [i 0]
                                                              (when (< i (.-length ticks))
                                                                (let [tick (aget ticks i)
                                                                      text (format-x-label (aget tick 1) style)
                                                                      prev (if (pos? (.-length labels))
                                                                             (.-text (aget labels
                                                                                           (dec (.-length labels))))
                                                                             nil)]
                                                                  (when (not= text prev)
                                                                    (let [label (make-text text 11 "#374151")]
                                                                      (.set (.-anchor label) 0.5 0)
                                                                      (.push labels label)
                                                                      (.push kept tick))))
                                                                (recur (inc i))))
                                                            (js-obj "labels" labels "ticks" kept)))
                                          add-x-axis (fn [rows pane geom]
                                                       (let [n      (aget geom "n")
                                                             cw     (aget geom "cw")
                                                             x0     (aget geom "x0")
                                                             axis-y (+ (aget geom "y0")
                                                                       (aget geom "ch"))
                                                             sx     (aget geom "sx")]
                                                         (when (and rows (> n 0))
                                                           (let [valid-ticks (fn [indices]
                                                                               (let [ticks (array)]
                                                                                 (loop [i 0]
                                                                                   (when (< i (.-length indices))
                                                                                     (let [idx   (aget indices i)
                                                                                           row   (aget rows idx)
                                                                                           v     (if (nil? row)
                                                                                                   nil
                                                                                                   (aget row "date"))
                                                                                           parts (if (nil? v)
                                                                                                   nil
                                                                                                   (date-parts v))]
                                                                                       (when (not (nil? parts))
                                                                                         (.push ticks (array idx parts)))
                                                                                       (recur (inc i)))))
                                                                                 ticks))
                                                                 style-of (fn [ticks]
                                                                            (x-label-style
                                                                             (aget (aget ticks 0) 1)
                                                                             (aget (aget ticks
                                                                                         (dec (.-length ticks))) 1)))
                                                                 place (fn [label idx]
                                                                         (let [w      (.-width label)
                                                                               center (js/Math.round
                                                                                       (if (>= w cw)
                                                                                         (+ x0 (/ w 2))
                                                                                         (js/Math.min
                                                                                          (js/Math.max (sx idx)
                                                                                                       (+ x0 (/ w 2)))
                                                                                          (- (+ x0 cw) (/ w 2)))))]
                                                                           (set! (.-x label) center)
                                                                           (set! (.-y label) (js/Math.round (+ axis-y 5)))
                                                                           center))
                                                                 layout (fn [ticks]
                                                                          (let [made   (make-x-labels ticks (style-of ticks))
                                                                                labels (aget made "labels")
                                                                                kept   (aget made "ticks")
                                                                                overlap? (loop [i 1]
                                                                                           (if (< i (.-length labels))
                                                                                             (let [prev (aget labels (dec i))
                                                                                                   cur  (aget labels i)
                                                                                                   pc   (place prev (aget (aget kept (dec i)) 0))
                                                                                                   cc   (place cur (aget (aget kept i) 0))]
                                                                                               (if (< (- (- cc (/ (.-width cur) 2))
                                                                                                         (+ pc (/ (.-width prev) 2)))
                                                                                                      6)
                                                                                                 true
                                                                                                 (recur (inc i))))
                                                                                             false))]
                                                                            (js-obj "labels" labels "ticks" kept
                                                                                    "overlap" overlap?)))
                                                                 draw (fn [made]
                                                                        (let [labels (aget made "labels")
                                                                              kept   (aget made "ticks")
                                                                              line   (new pixi/Graphics)
                                                                              marks  (new pixi/Graphics)]
                                                                          (loop [i 0]
                                                                            (when (< i (.-length labels))
                                                                              (place (aget labels i)
                                                                                     (aget (aget kept i) 0))
                                                                              (recur (inc i))))
                                                                          (.moveTo line x0 axis-y)
                                                                          (.lineTo line (+ x0 cw) axis-y)
                                                                          (.stroke line (js-obj "width" 1
                                                                                                "color" "#9ca3af"))
                                                                          (.addChild pane line)
                                                                          (loop [i 0]
                                                                            (when (< i (.-length kept))
                                                                              (let [x (sx (aget (aget kept i) 0))]
                                                                                (.moveTo marks x axis-y)
                                                                                (.lineTo marks x (+ axis-y 3)))
                                                                              (recur (inc i))))
                                                                          (.stroke marks (js-obj "width" 1
                                                                                                 "color" "#9ca3af"))
                                                                          (.addChild pane marks)
                                                                          (loop [i 0]
                                                                            (when (< i (.-length labels))
                                                                              (.addChild pane (aget labels i))
                                                                              (recur (inc i))))))]
                                                             (loop [indices (x-tick-indices n cw)]
                                                               (let [ticks (valid-ticks indices)
                                                                     made  (when (pos? (.-length ticks))
                                                                             (layout ticks))]
                                                                 (cond
                                                                   (nil? made)
                                                                   (when (> (.-length indices) 2)
                                                                     (recur (tick-indices
                                                                             n (dec (.-length indices)))))

                                                                   (aget made "overlap")
                                                                   (if (> (.-length indices) 2)
                                                                     (recur (tick-indices
                                                                             n (dec (.-length indices))))
                                                                     (let [tick  (aget ticks (dec (.-length ticks)))
                                                                           label (make-text
                                                                                  (format-x-label
                                                                                   (aget tick 1)
                                                                                   (style-of ticks))
                                                                                  11 "#374151")]
                                                                       (.set (.-anchor label) 0.5 0)
                                                                       (place label (aget tick 0))
                                                                       (draw (js-obj
                                                                              "labels" (array label)
                                                                              "ticks" (array tick)
                                                                              "overlap" false))))

                                                                   :else (draw made))))))))
                                          add-line (fn [graphics rows geom mn mx col color]
                                                     (let [n (.-length rows)]
                                                       (when (and (js/isFinite mn) (js/isFinite mx))
                                                         (loop [i 0 started false]
                                                           (when (< i n)
                                                             (let [y (get-y geom mn mx rows col i)]
                                                               (if (nil? y)
                                                                 (recur (inc i) started)
                                                                 (do
                                                                   (if started
                                                                     (.lineTo graphics (get-x geom i) y)
                                                                     (.moveTo graphics (get-x geom i) y))
                                                                   (recur (inc i) true))))))
                                                         (.stroke graphics
                                                                  (js-obj "width" 2 "color" color)))))
                                          add-points (fn [graphics rows geom mn mx col color]
                                                       (let [n (.-length rows)]
                                                         (when (and (js/isFinite mn) (js/isFinite mx))
                                                           (loop [i 0]
                                                             (when (< i n)
                                                               (let [y (get-y geom mn mx rows col i)]
                                                                 (when (not (nil? y))
                                                                   (.circle graphics (get-x geom i) y 2))
                                                                 (recur (inc i)))))
                                                           (.fill graphics (js-obj "color" color)))))
                                          add-range (fn [graphics rows geom mn mx col1 col2 color]
                                                      (let [n     (.-length rows)
                                                            xstep (aget geom "xstep")]
                                                        (when (and (js/isFinite mn) (js/isFinite mx))
                                                          (loop [i 0]
                                                            (when (< i n)
                                                              (let [y1 (get-y geom mn mx rows col1 i)
                                                                    y2 (get-y geom mn mx rows col2 i)]
                                                                (when (and (not (nil? y1)) (not (nil? y2)))
                                                                  (let [top (js/Math.min y1 y2)
                                                                        ht  (js/Math.abs (- y1 y2))]
                                                                    (.rect graphics
                                                                           (- (get-x geom i) (/ xstep 2))
                                                                           top xstep ht))))
                                                              (recur (inc i)))))
                                                        (.fill graphics
                                                               (js-obj "color" color "alpha" 0.5))))
                                          add-signal (fn [graphics rows geom col color]
                                                       (let [n (.-length rows)
                                                             y (+ (aget geom "y0") (aget geom "ch"))]
                                                         (loop [i 0]
                                                           (when (< i n)
                                                             (when (aget (aget rows i) col)
                                                               (.circle graphics (get-x geom i) y 2))
                                                             (recur (inc i))))
                                                         (.fill graphics
                                                                (js-obj "color" color "alpha" 0.6))))
                                          add-bars (fn [graphics rows geom mn mx col]
                                                     (let [c0        (aget col 0)
                                                           c1        (aget col 1)
                                                           c2        (aget col 2)
                                                           c3        (aget col 3)
                                                           n         (.-length rows)
                                                           xstep     (aget geom "xstep")
                                                           bar-width (max 1 (- xstep 2))]
                                                       (when (and (js/isFinite mn) (js/isFinite mx))
                                                         (loop [i 0]
                                                           (when (< i n)
                                                             (let [row (aget rows i)
                                                                   o   (get-y geom mn mx rows c0 i)
                                                                   hi  (get-y geom mn mx rows c1 i)
                                                                   lo  (get-y geom mn mx rows c2 i)
                                                                   c   (get-y geom mn mx rows c3 i)]
                                                               (when (and (not (nil? o)) (not (nil? hi))
                                                                          (not (nil? lo)) (not (nil? c)))
                                                                 (let [x-center (get-x geom i)
                                                                       x        (- x-center (/ bar-width 2))
                                                                       upper    (js/Math.min o c)
                                                                       lower    (js/Math.max o c)
                                                                       bar-h    (js/Math.abs (- o c))
                                                                       color    (if (>= (aget row c3)
                                                                                        (aget row c0))
                                                                                  "#66ccff" "#ff3333")]
                                                                   (.rect graphics x upper bar-width bar-h)
                                                                   (.fill graphics (js-obj "color" color))
                                                                   (.stroke graphics (js-obj "width" 1
                                                                                             "color" "#ffffff"))
                                                                   (.moveTo graphics x-center upper)
                                                                   (.lineTo graphics x-center hi)
                                                                   (.stroke graphics (js-obj "width" 1 "color" color))
                                                                   (.moveTo graphics x-center lower)
                                                                   (.lineTo graphics x-center lo)
                                                                   (.stroke graphics (js-obj "width" 1 "color" color)))))
                                                             (recur (inc i)))))))
                                          draw-series (fn [rows pane geom mn mx series]
                                                        (when series
                                                          (let [t     (aget series "type")
                                                                col   (aget series "col")
                                                                color (or (aget series "color") "#3b82f6")
                                                                g     (new pixi/Graphics)]
                                                            (cond
                                                              (= t "line")
                                                              (add-line g rows geom mn mx col color)

                                                              (= t "point")
                                                              (add-points g rows geom mn mx col color)

                                                              (= t "range")
                                                              (add-range g rows geom mn mx
                                                                         (aget col 0) (aget col 1) color)

                                                              (= t "signal")
                                                              (add-signal g rows geom col color)

                                                              (= t "bar")
                                                              (add-bars g rows geom mn mx
                                                                        (let [c (aget series "col")]
                                                                          (if (and (array? c)
                                                                                   (>= (.-length c) 4))
                                                                            c
                                                                            (array "open" "high"
                                                                                   "low" "close"))))

                                                              :else nil)
                                                            (.addChild pane g))))
                                          draw-chart (fn [rows series y-offset height show-x-axis?]
                                                       (let [pane     (new pixi/Container)
                                                             bg       (new pixi/Graphics)
                                                             w        (.-width state)
                                                             pad      0
                                                             inset    3
                                                             bw       (max 1 (- w (* 2 pad)))
                                                             bh       (max 1 (- height (* 2 pad)))
                                                             y0       (+ pad inset)
                                                             x-gutter (if show-x-axis? 20 0)
                                                             ch       (max 1 (- bh (* 2 inset) x-gutter))
                                                             ns       (if series (.-length series) 0)
                                                             bnds     (loop [i 0 mn js/Infinity mx (- js/Infinity)]
                                                                        (if (< i ns)
                                                                          (let [b (bounds-of rows (aget series i))]
                                                                            (recur (inc i)
                                                                                   (js/Math.min mn (aget b 0))
                                                                                   (js/Math.max mx (aget b 1))))
                                                                          (array mn mx)))
                                                             mn       (aget bnds 0)
                                                             mx       (aget bnds 1)
                                                             finite?  (and (js/isFinite mn) (js/isFinite mx))
                                                             ticks    (if finite? (tick-values mn mx ch) nil)
                                                             labels   (if finite? (make-labels ticks) nil)
                                                             widest   (if finite?
                                                                        (loop [i 0 mw 0]
                                                                          (if (< i (.-length labels))
                                                                            (recur (inc i)
                                                                                   (js/Math.max
                                                                                    mw (.-width (aget labels i))))
                                                                            mw))
                                                                        0)
                                                             gutter   (if finite?
                                                                        (js/Math.min (* w 0.3)
                                                                                     (max 24 (+ widest 10)))
                                                                        0)
                                                             axis-x   (+ pad gutter)
                                                             x0       (+ axis-x inset)
                                                             cw       (max 1 (- (- w pad inset) x0))
                                                             n        (.-length rows)
                                                             xstep    (/ cw (max 1 n))
                                                             sx       (fn [i] (+ x0 (* (+ i 0.5) xstep)))
                                                             sy       (fn [v mn mx]
                                                                        (if (or (not (number? v))
                                                                                (js/isNaN v))
                                                                          nil
                                                                          (if (= mx mn)
                                                                            (+ y0 (/ ch 2))
                                                                            (+ y0 (* (- 1 (/ (- v mn)
                                                                                             (- mx mn)))
                                                                                     ch)))))
                                                             geom     (js-obj "sx" sx "sy" sy
                                                                              "x0" x0 "y0" y0
                                                                              "cw" cw "ch" ch
                                                                              "axis-x" axis-x
                                                                              "xstep" xstep "n" n)]
                                                         (set! (.-y pane) y-offset)
                                                         (.rect bg pad pad bw bh)
                                                         (.fill bg (js-obj "color" "#f5f5f5"))
                                                         (.addChild pane bg)
                                                         (add-y-axis pane geom mn mx ticks labels)
                                                         (when show-x-axis?
                                                           (add-x-axis rows pane geom))
                                                         (when series
                                                           (loop [i 0]
                                                             (when (< i ns)
                                                               (draw-series rows pane geom mn mx
                                                                            (aget series i))
                                                               (recur (inc i)))))
                                                         (.addChild container pane)
                                                         pane))
                                          draw-date (fn [rows pane]
                                                      (let [n (.-length rows)]
                                                        (when (> n 0)
                                                          (let [row  (aget rows (dec n))
                                                                date (aget row "date")
                                                                text (make-text (str date) 12 "#262626")]
                                                            (set! (.-x text) (js/Math.round (max 10 (- (.-width state) 170))))
                                                            (set! (.-y text) 22)
                                                            (.addChild pane text)))))
                                          redraw (fn []
                                                   (let [rows      (.-data state)
                                                         h         (.-height state)
                                                         h1        (/ h 2)
                                                         h2        (/ h 4)
                                                         h3        (- h h1 h2)
                                                         charts    (.-charts state)
                                                         container (.-container state)]
                                                     (.removeChildren container)
                                                     (let [pane1 (draw-chart rows (aget charts 0) 0 h1 true)]
                                                       (when (> (.-length charts) 1)
                                                         (draw-chart rows (aget charts 1) h1 h2 false))
                                                       (when (> (.-length charts) 2)
                                                         (draw-chart rows (aget charts 2) (+ h1 h2) h3 false))
                                                       (draw-date rows pane1))))]
                                      (.addChild stage container)
                                      (set! (.-display (.-style (.-canvas app))) "block")
                                      (.appendChild el (.-canvas app))
                                      (redraw)))))))))]
             (set! (.-init ctx) init!)
             (init!))))
  (update [_root]
          (set! (.-options ctx) options)
          (let [app (.-app ctx)
                g   (inc (or (.-gen ctx) 0))]
            (set! (.-gen ctx) g)
            (when app (try (.destroy app true true) (catch :default _ nil)))
            ((.-init ctx))))
  (unmount [_]
           (set! (.-gen ctx) (inc (or (.-gen ctx) 0)))
           (when-let [err (.-err ctx)]
             (set! (.-display (.-style err)) "none"))
           (when-let [app (.-app ctx)]
             (try (.destroy app true true) (catch :default _ nil)))))

(defn- resolve-color
  "Resolve a series :color name to a CSS color; default when absent."
  [c]
  (or (colors/set-color (or c "blue-5")) "#3b82f6"))

(defn- resolve-charts
  "Resolve every plot's :color name to a concrete CSS color, server-side, so
   the client component needs no color table."
  [charts]
  (when charts
    (mapv (fn [pane]
            (mapv (fn [series]
                    (if (map? series)
                      (update series :color resolve-color)
                      series))
                  pane))
          charts)))

(defn- normalize-date-value
  "Coerce a :date value to something the JSON payload can carry: strings and
   numbers pass through unchanged, other objects (java.time types,
   java.util.Date, ...) become their str form (ISO-like) so the client-side
   x-axis parser sees a string."
  [v]
  (if (or (nil? v) (string? v) (number? v)) v (str v)))

(defn- normalize-dates
  "Normalize the :date column of every row to a string or number for the
   client component; rows without a :date key are left untouched."
  [rows]
  (when rows
    (mapv (fn [row]
            (if (and (map? row) (contains? row :date))
              (update row :date normalize-date-value)
              row))
          rows)))

(defn- valid-size?
  "True for a positive integer dimension."
  [x]
  (and (integer? x) (pos? x)))

(defn- window-size-style
  "Exact pixel host dimensions from a :window with valid integer width and
   height, regardless of mode; nil otherwise."
  [window]
  (let [w (:width window)
        h (:height window)]
    (when (and (valid-size? w) (valid-size? h))
      {:width (str w "px") :height (str h "px")})))

(def ^:private default-window
  "Fallback host size for the generic to-hiccup path.  The model and interact
   paths always supply their own window through the protocol."
  {:width 900 :height 600})

(defn- render
  "Render selected ROWS with charts SPEC at WINDOW.  SPEC colors are resolved
   server-side and dates normalized before the payload reaches the client."
  [rows spec window]
  (itera-pixi-timeseries
   {:options (cond-> {:data (normalize-dates rows) :window window}
               spec (assoc :charts (resolve-charts spec)))
    :style   (merge {:display "block" :width "100%" :height "100%"
                     :overflow "hidden"}
                    (window-size-style window))}))

(defn- formatted-hiccup
  "Render ROWS with SPEC (or the smart default when SPEC is nil) at WINDOW.
   Invalid specs render fmt/invalid-spec-block."
  [rows spec window]
  (if-let [spec (fmt/normalize-spec (or spec (fmt/default-spec* rows)))]
    (render rows spec window)
    fmt/invalid-spec-block))

(defrecord PixiTimeseries [data]
  protocol/itera-convertable
  (to-hiccup [_]
    (formatted-hiccup data nil default-window))

  protocol/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (formatted-hiccup data spec window))
  (default-spec [_]
    (fmt/default-spec* data))
  (spec-editor [_ spec code-a]
    (fmt/pixi-timeseries-format-edit-ui data {:spec spec :code-a code-a}))

  protocol/itera-page-plot
  (to-formatted-paged-hiccup [v spec window page]
    (let [page (or page (protocol/init-page v spec window))
          rows (select-rows {:window window :window-selector page} data)]
      (formatted-hiccup rows spec window)))
  (init-page [_ _spec window]
    (let [a (atom {:window window
                   :window-selector {:display :nice :step 8}})]
      (init-window-selector! a data)
      (:window-selector @a)))
  (page-editor [v spec window page-a]
    (page-editor/page-editor-view v spec window page-a)))

(defn pixi-timeseries
  "Return a pixi-timeseries viewer over ROWS (a vector of row maps with :date).
   The charts spec and size are supplied by the itera.value.protocol methods:
   `to-formatted-hiccup` takes {:panes...} + {:width :height}, `to-hiccup` uses
   the smart default at a fixed fallback size."
  [rows]
  (->PixiTimeseries (vec (or rows []))))

(defn pixi-timeseries-ds
  "Return a pixi-timeseries value from a dataset-like DS (rows or column map)."
  [ds]
  (pixi-timeseries (ds->rows ds)))
