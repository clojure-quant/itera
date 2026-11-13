(ns itera.plot.pixi-timeseries
  "pixi.js time-series viewer as a Hyper client component.

   opts:
     :data    rows (vector of maps) or a column map; required
     :charts  vector of panes, each a vector of series maps:
              {:type :line|:point|:range|:signal
               :col  <keyword> or [<kw> <kw>] for :range
               :color \"blue-5\" (tailwind name or CSS color)}
     :step    pixels per row (default 8)
     :style   host element CSS, e.g. {:width \"100%\" :height \"800px\"}
     :class   host element class

   `pixi-timeseries-ds` accepts a dataset-like value (tech.ml dataset when
   available, otherwise rows or a column map) and delegates to
   `pixi-timeseries`.  Column maps whose columns have different lengths are
   truncated to the shortest column."
  (:require [hyper.core :as h]))

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
   JSON-parsed viewer payload (`:data`, `:charts`, `:step`)."
  {:require [["https://esm.sh/pixi.js@8.6.6?bundle" :as pixi]]}
  [{:keys [options]}]
  (render
   [:div {:style "display:flex;flex-direction:column;width:100%;height:100%"}
    [:div {:class "pixi-canvas" :style "flex:1;min-height:0;position:relative"}]
    [:input {:class "pixi-nav" :type "range"
             :style "width:100%;height:24px;margin:0"}]])
  (mount [root]
    (let [el (.querySelector root ".pixi-canvas")
          slider (.querySelector root ".pixi-nav")]
      (set! (.-el ctx) el)
      (set! (.-slider ctx) slider)
      (set! (.-options ctx) options)
      (set! (.-gen ctx) 0)
      (let [init!
            (fn []
              (let [opts   (.-options ctx)
                    el     (.-el ctx)
                    slider (.-slider ctx)
                    cw     (.-clientWidth el)
                    ch     (.-clientHeight el)
                    w      (if (> cw 0) cw 800)
                    h      (if (> ch 0) ch 600)
                    app    (new pixi/Application)
                    g      (inc (or (.-gen ctx) 0))]
                (set! (.-gen ctx) g)
                (set! (.-app ctx) app)
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
                                 colors    (js-obj "blue-3" "#93c5fd"
                                                   "blue-4" "#60a5fa"
                                                   "blue-5" "#3b82f6"
                                                   "blue-6" "#2563eb"
                                                   "blue-7" "#1d4ed8"
                                                   "blue-8" "#1e40af"
                                                   "red-3" "#fca5a5"
                                                   "red-4" "#f87171"
                                                   "red-5" "#ef4444"
                                                   "red-6" "#dc2626"
                                                   "red-7" "#b91c1c"
                                                   "red-8" "#991b1b"
                                                   "green-2" "#bbf7d0"
                                                   "green-3" "#86efac"
                                                   "green-4" "#4ade80"
                                                   "green-5" "#22c55e"
                                                   "green-6" "#16a34a"
                                                   "green-7" "#15803d"
                                                   "green-8" "#166534"
                                                   "green-9" "#14532d"
                                                   "neutral-1" "#f5f5f5")
                                 set-color (fn [c]
                                             (or (aget colors c) c "blue-5"))
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
                                 scale-value (fn [v mn mx height]
                                               (if (or (not (number? v)) (js/isNaN v))
                                                 nil
                                                 (let [rng (- mx mn)]
                                                   (if (= rng 0)
                                                     (/ height 2)
                                                     (* (- 1 (/ (- v mn) rng))
                                                        height)))))
                                 add-line (fn [graphics rows height col color]
                                            (let [mn   (true-min rows col)
                                                  mx   (true-max rows col)
                                                  n    (.-length rows)
                                                  st   (.-step state)]
                                              (when (and (js/isFinite mn) (js/isFinite mx))
                                                (loop [i 0 started false]
                                                  (when (< i n)
                                                    (let [y (scale-value (aget (aget rows i) col)
                                                                         mn mx height)]
                                                      (if (nil? y)
                                                        (recur (inc i) started)
                                                        (do
                                                          (if started
                                                            (.lineTo graphics (* i st) y)
                                                            (.moveTo graphics (* i st) y))
                                                          (recur (inc i) true))))))
                                                (.stroke graphics
                                                         (js-obj "width" 2 "color" color)))))
                                 add-points (fn [graphics rows height col color]
                                              (let [mn (true-min rows col)
                                                    mx (true-max rows col)
                                                    n  (.-length rows)
                                                    st (.-step state)]
                                                (when (and (js/isFinite mn) (js/isFinite mx))
                                                  (loop [i 0]
                                                    (when (< i n)
                                                      (let [y (scale-value (aget (aget rows i) col)
                                                                           mn mx height)]
                                                        (when (not (nil? y))
                                                          (.circle graphics (* i st) y 2))
                                                        (recur (inc i)))))
                                                  (.fill graphics (js-obj "color" color)))))
                                 add-range (fn [graphics rows height col1 col2 color]
                                             (let [mn (js/Math.min (true-min rows col1)
                                                                   (true-min rows col2))
                                                   mx (js/Math.max (true-max rows col1)
                                                                   (true-max rows col2))
                                                   n  (.-length rows)
                                                   st (.-step state)]
                                               (when (and (js/isFinite mn) (js/isFinite mx))
                                                 (loop [i 0]
                                                   (when (< i n)
                                                     (let [row (aget rows i)
                                                           y1  (scale-value (aget row col1)
                                                                            mn mx height)
                                                           y2  (scale-value (aget row col2)
                                                                            mn mx height)]
                                                       (when (and (not (nil? y1)) (not (nil? y2)))
                                                         (let [top (js/Math.min y1 y2)
                                                               ht  (js/Math.abs (- y1 y2))]
                                                           (.rect graphics (* i st) top st ht))))
                                                     (recur (inc i)))))
                                                 (.fill graphics
                                                        (js-obj "color" color "alpha" 0.5))))
                                 add-signal (fn [graphics rows height col color]
                                              (let [n  (.-length rows)
                                                    y  (- height 50)
                                                    st (.-step state)]
                                                (loop [i 0]
                                                  (when (< i n)
                                                    (when (aget (aget rows i) col)
                                                      (.circle graphics (* i st) y 2))
                                                    (recur (inc i))))
                                                (.fill graphics
                                                       (js-obj "color" color "alpha" 0.6))))
                                 draw-series (fn [rows pane height series]
                                               (when series
                                                 (let [t     (aget series "type")
                                                       col   (aget series "col")
                                                       color (set-color
                                                              (or (aget series "color") "blue-5"))
                                                       g     (new pixi/Graphics)]
                                                   (cond
                                                     (= t "line")
                                                     (add-line g rows height col color)

                                                     (= t "point")
                                                     (add-points g rows height col color)

                                                     (= t "range")
                                                     (add-range g rows height
                                                                (aget col 0) (aget col 1) color)

                                                     (= t "signal")
                                                     (add-signal g rows height col color)

                                                     :else nil)
                                                   (.addChild pane g))))
                                 draw-chart (fn [rows series y-offset height]
                                              (let [pane      (new pixi/Container)
                                                    bg        (new pixi/Graphics)
                                                    st        (.-step state)
                                                    bg-width  (max 1 (- (* (.-visible state) st) 20))
                                                    bg-height (max 1 (- height 20))]
                                                (set! (.-y pane) y-offset)
                                                (.rect bg 10 10 bg-width bg-height)
                                                (.fill bg (js-obj "color" (set-color "neutral-1")))
                                                (.addChild pane bg)
                                                (when series
                                                  (let [n (.-length series)]
                                                    (loop [i 0]
                                                      (when (< i n)
                                                        (draw-series rows pane height (aget series i))
                                                        (recur (inc i))))))
                                                (.addChild container pane)
                                                pane))
                                 draw-bars (fn [rows pane height]
                                             (let [mn        (true-min rows "low")
                                                   mx        (true-max rows "high")
                                                   n         (.-length rows)
                                                   st        (.-step state)
                                                   bar-width (- st 2)
                                                   g         (new pixi/Graphics)]
                                               (when (and (js/isFinite mn) (js/isFinite mx))
                                                 (loop [i 0]
                                                   (when (< i n)
                                                     (let [row (aget rows i)
                                                           o   (scale-value (aget row "open") mn mx height)
                                                           hi  (scale-value (aget row "high") mn mx height)
                                                           lo  (scale-value (aget row "low") mn mx height)
                                                           c   (scale-value (aget row "close") mn mx height)]
                                                       (when (and (not (nil? o)) (not (nil? hi))
                                                                  (not (nil? lo)) (not (nil? c)))
                                                         (let [x-center (* i st)
                                                               x        (- x-center (/ bar-width 2))
                                                               upper    (js/Math.min o c)
                                                               lower    (js/Math.max o c)
                                                               bar-h    (js/Math.abs (- o c))
                                                               color    (if (>= (aget row "close")
                                                                                (aget row "open"))
                                                                          "#66ccff" "#ff3333")]
                                                           (.rect g x upper bar-width bar-h)
                                                           (.fill g (js-obj "color" color))
                                                           (.stroke g (js-obj "width" 1
                                                                              "color" "#ffffff"))
                                                           (.moveTo g x-center upper)
                                                           (.lineTo g x-center hi)
                                                           (.stroke g (js-obj "width" 1 "color" color))
                                                           (.moveTo g x-center lower)
                                                           (.lineTo g x-center lo)
                                                           (.stroke g (js-obj "width" 1 "color" color)))))
                                                     (recur (inc i)))))
                                               (.addChild pane g)))
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
                                              (draw-bars rows pane1 h1)
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
                             (redraw)))))))]
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
    (when-let [app (.-app ctx)]
      (try (.destroy app true true) (catch :default _ nil)))))

(defn pixi-timeseries
  [{:keys [style class rows] :as opts}]
  (itera-pixi-timeseries
   (cond-> {:options (cond-> (dissoc opts :style :class :rows)
                       rows (assoc :data rows))}
     style (assoc :style (merge {:display "block"
                                 :width "100%"
                                 :height "100%"}
                                style))
     class (assoc :class class))))

(defn pixi-timeseries-ds
  [opts ds]
  (pixi-timeseries (assoc opts :data (ds->rows ds))))
