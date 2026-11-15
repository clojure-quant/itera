(ns itera.plot.pixi-timeseries
  "pixi.js time-series viewer as a Hyper client component.

   opts:
     :data    rows (vector of maps) or a column map; required
     :charts  vector of panes, each a vector of series maps:
              {:type one of itera.plot.pixi.plot-type/types
               :col  <kw> or [<kw> <kw>] for :range
                     or [<open> <high> <low> <close>] for :bar
               :color \"blue-5\" (name in itera.plot.color/colors or a CSS color)}
     :step    pixels per row (default 8)
     :window  plot size, a map:
              {:mode :container|:fixed :width <px> :height <px>}
              :width and :height must be positive integers in both modes;
              pixi never measures the DOM.  With :mode :container the
              dimensions are supplied by itera.container's measurement loop,
              so the caller/page layer must withhold the viewer until that
              loop has measured a complete window.  Absent or invalid
              dimensions render \"invalid :width and :height\" and draw
              nothing.  The fixed 24 px slider is subtracted from the window
              height before the canvas is sized.
     :style   host element CSS, e.g. {:width \"100%\" :height \"800px\"}
     :class   host element class

   Named colors are resolved to hex server-side with itera.plot.color before
   the payload reaches the browser: the component below is compiled to
   client-side JS and cannot call the JVM color namespace itself.

   `pixi-timeseries-ds` accepts a dataset-like value (tech.ml dataset when
   available, otherwise rows or a column map) and delegates to
   `pixi-timeseries`.  Column maps whose columns have different lengths are
   truncated to the shortest column."
  (:require [hyper.core :as h]
            [itera.plot.color :as colors]))

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

(h/defc itera-pixi-timeseries
  "pixi.js time-series viewer in a custom element. `options` is the
   JSON-parsed viewer payload (`:data`, `:charts`, `:step`, `:window`)."
  {:require [["https://esm.sh/pixi.js@8.6.6?bundle" :as pixi]]}
  [{:keys [options]}]
  (render
   [:div {:style "display:flex;flex-direction:column;width:100%;height:100%"}
    [:div {:class "pixi-canvas" :style "flex:1;min-height:0;position:relative"}]
    [:div {:class "pixi-error"
           :style "display:none;flex:1;min-height:0;align-items:center;justify-content:center;color:#b91c1c;font-size:0.85rem"}
     "invalid :width and :height"]
    [:input {:class "pixi-nav" :type "range"
             :style "width:100%;height:24px;margin:0"}]])
  (mount [root]
    (let [el (.querySelector root ".pixi-canvas")
          err (.querySelector root ".pixi-error")
          slider (.querySelector root ".pixi-nav")]
      (set! (.-el ctx) el)
      (set! (.-err ctx) err)
      (set! (.-slider ctx) slider)
      (set! (.-options ctx) options)
      (set! (.-gen ctx) 0)
      (let [init!
            (fn []
              (let [opts   (.-options ctx)
                    el     (.-el ctx)
                    err    (.-err ctx)
                    slider (.-slider ctx)
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
                    (set! (.-display (.-style slider)) "none")
                    (set! (.-display (.-style err)) "flex")
                    (set! (.-oninput slider) nil)
                    (set! (.-app ctx) nil))
                  (let [w      ww
                        h      (max 1 (- wh 24))
                        app    (new pixi/Application)
                        g      (inc (or (.-gen ctx) 0))]
                    (set! (.-gen ctx) g)
                    (set! (.-app ctx) app)
                    (set! (.-display (.-style err)) "none")
                    (set! (.-display (.-style el)) "")
                    (set! (.-display (.-style slider)) "")
                    (set! (.-innerHTML el) "")
                    (.then (.init app (js-obj "width" w "height" h
                                              "background" "#1099bb"
                                              "antialias" true))
                       (fn [_]
                         (when (= g (.-gen ctx))
                           (let [data      (or (aget opts "data") (array))
                                 charts    (or (aget opts "charts") (array))
                                 step      (or (aget opts "step") 8)
                                 row-count (.-length data)
                                 visible   (min row-count
                                                (max 1 (js/Math.floor (/ w step))))
                                 stage     (.-stage app)
                                 container (new pixi/Container)
                                 state     (js-obj "width" w "height" h
                                                   "step" step
                                                   "data" data "charts" charts
                                                   "end" row-count
                                                   "visible" visible
                                                   "container" container
                                                   "slider" slider)
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
                                 draw-chart (fn [rows series y-offset height]
                                              (let [pane  (new pixi/Container)
                                                    bg    (new pixi/Graphics)
                                                    w     (.-width state)
                                                    pad   10
                                                    inset 3
                                                    bw    (max 1 (- w (* 2 pad)))
                                                    bh    (max 1 (- height (* 2 pad)))
                                                    x0    (+ pad inset)
                                                    y0    (+ pad inset)
                                                    cw    (max 1 (- bw (* 2 inset)))
                                                    ch    (max 1 (- bh (* 2 inset)))
                                                    n     (.-length rows)
                                                    xstep (/ cw (max 1 n))
                                                    sx    (fn [i] (+ x0 (* (+ i 0.5) xstep)))
                                                    sy    (fn [v mn mx]
                                                            (if (or (not (number? v))
                                                                    (js/isNaN v))
                                                              nil
                                                              (if (= mx mn)
                                                                (+ y0 (/ ch 2))
                                                                (+ y0 (* (- 1 (/ (- v mn)
                                                                                 (- mx mn)))
                                                                         ch)))))
                                                     geom  (js-obj "sx" sx "sy" sy
                                                                   "x0" x0 "y0" y0
                                                                   "cw" cw "ch" ch
                                                                   "xstep" xstep "n" n)
                                                    ns    (if series (.-length series) 0)
                                                    bnds  (loop [i 0 mn js/Infinity mx (- js/Infinity)]
                                                            (if (< i ns)
                                                              (let [b (bounds-of rows (aget series i))]
                                                                (recur (inc i)
                                                                       (js/Math.min mn (aget b 0))
                                                                       (js/Math.max mx (aget b 1))))
                                                              (array mn mx)))
                                                    mn    (aget bnds 0)
                                                    mx    (aget bnds 1)]
                                                (set! (.-y pane) y-offset)
                                                (.rect bg pad pad bw bh)
                                                (.fill bg (js-obj "color" "#f5f5f5"))
                                                (.addChild pane bg)
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
                                                       text (new pixi/Text
                                                                 (js-obj
                                                                  "text" (str date)
                                                                  "style" (js-obj "fontSize" 12
                                                                                  "fill" "#262626")))]
                                                   (set! (.-x text) (max 10 (- (.-width state) 170)))
                                                   (set! (.-y text) 22)
                                                   (.addChild pane text)))))
                                 navigate (fn [op value]
                                            (let [rc  (.-length (.-data state))
                                                  vis (.-visible state)
                                                  cur (.-end state)
                                                  ne  (cond
                                                        (= op "begin") vis
                                                        (= op "end")   rc
                                                        (= op "prior") (max vis (- cur vis))
                                                        (= op "next")  (min rc (+ cur vis))
                                                        (= op "idx")   (js/Math.round value)
                                                        :else          cur)
                                                  ne  (max vis (min rc ne))
                                                  sl  (.-slider state)]
                                              (set! (.-end state) ne)
                                              (when (not= (.-value sl) (str ne))
                                                (set! (.-value sl) (str ne)))
                                              ((.-redraw state))))
                                 add-button (fn [pane pane-height op label x]
                                              (let [y (- pane-height 40)
                                                    g (new pixi/Graphics)
                                                    t (new pixi/Text
                                                             (js-obj
                                                              "text" label
                                                              "style" (js-obj "fontSize" 12
                                                                              "fill" "#ffffff")))]
                                                (.rect g x y 30 30)
                                                (.fill g (js-obj "color" "#aa4f08"))
                                                (set! (.-eventMode g) "static")
                                                (set! (.-cursor g) "pointer")
                                                (.on g "pointerdown" (fn [_] ((.-navigate state) op)))
                                                (set! (.-x t) (+ x 8))
                                                (set! (.-y t) (+ y 8))
                                                (.addChild pane g)
                                                (.addChild pane t)))
                                 draw-buttons (fn [pane pane-height]
                                                (let [x-base (max 10 (- (.-width state) 160))]
                                                  (add-button pane pane-height "begin" "<|" x-base)
                                                  (add-button pane pane-height "prior" "<" (+ x-base 38))
                                                  (add-button pane pane-height "next" ">" (+ x-base 76))
                                                  (add-button pane pane-height "end" ">|" (+ x-base 114))))
                                 redraw (fn []
                                          (let [data      (.-data state)
                                                end       (.-end state)
                                                vis       (.-visible state)
                                                start     (max 0 (- end vis))
                                                rows      (.slice data start end)
                                                h         (.-height state)
                                                h1        (/ h 2)
                                                h2        (/ h 4)
                                                h3        (- h h1 h2)
                                                charts    (.-charts state)
                                                container (.-container state)]
                                            (.removeChildren container)
                                            (let [pane1 (draw-chart rows (aget charts 0) 0 h1)]
                                              (when (> (.-length charts) 1)
                                                (draw-chart rows (aget charts 1) h1 h2))
                                              (when (> (.-length charts) 2)
                                                (draw-chart rows (aget charts 2) (+ h1 h2) h3))
                                              (draw-date rows pane1)
                                              (draw-buttons pane1 h1))))]
                             (.addChild stage container)
                             (set! (.-min slider) (str visible))
                             (set! (.-max slider) (str row-count))
                             (set! (.-value slider) (str row-count))
                             (set! (.-navigate state) navigate)
                             (set! (.-redraw state) redraw)
                             (set! (.-oninput slider)
                                   (fn [_]
                                     ((.-navigate state) "idx"
                                      (js/parseInt (.-value slider)))))
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
    (when-let [sl (.-slider ctx)]
      (set! (.-oninput sl) nil))
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

(defn pixi-timeseries
  "Render a pixi.js time-series viewer.

   :window :width/:height must be positive integers in both modes; the
   viewer never measures the DOM.  With :mode :container the dimensions are
   supplied by itera.container's measurement loop, so the caller (typically
   itera.page.algo/algo-page) must withhold this viewer until that loop has
   measured a complete window.  Invalid dimensions render
   \"invalid :width and :height\" and draw nothing.  A caller-supplied :style
   wins over the :window-derived host size."
  [{:keys [style class rows window] :as opts}]
  (let [options (cond-> (dissoc opts :style :class :rows)
                  rows           (assoc :data rows)
                  (:charts opts) (update :charts resolve-charts))
        host-style (merge {:display "block"
                           :width "100%"
                           :height "100%"}
                          (window-size-style window)
                          style)]
    (itera-pixi-timeseries
     (cond-> {:options options :style host-style}
       class (assoc :class class)))))

(defn pixi-timeseries-ds
  [opts ds]
  (pixi-timeseries (assoc opts :data (ds->rows ds))))
