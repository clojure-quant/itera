(ns itera.container
  "Measured plot container: a client-side component that emits its own size
   and keeps an algo's plot-options cursor carrying a normalized :window map.

   :window is {:mode :container|:fixed :width <px> :height <px>}, where the
   dimensions are positive integers:
   - :container (default) — the measured clientWidth/clientHeight are written
     back into :window as the container resizes.  On a fresh mount the
     dimensions may be omitted until the first measurement arrives.
- :fixed — caller-supplied :width/:height are authoritative and the
      measurement action leaves :window unchanged."
  (:require [hyper.core :as h]))

;; --- window normalization ---------------------------------------------------

(defn- positive-integer? [x]
  (and (integer? x) (pos? x)))

(defn- ->mode [m]
  (cond
    (keyword? m) m
    (string? m)  (keyword m)
    :else        nil))

(defn normalize-window
  "Normalize a :window value.

   Dimensions are positive integers.

   - nil/absent                     -> {:mode :container}
   - explicit :mode is kept as a keyword
   - no :mode + positive width and height -> {:mode :fixed :width w :height h}
   - :fixed without both dimensions -> {:mode :container}
   - anything else                  -> {:mode :container}

   A :container window may temporarily omit width/height (before the first
   measurement) and keeps any positive integer width/height (the last
   measurement) so the editor can display them."
  [w]
  (let [w      (if (map? w) w {})
        mode   (->mode (:mode w))
        width  (:width w)
        height (:height w)
        width  (when (positive-integer? width) width)
        height (when (positive-integer? height) height)
        fixed? (and width height)]
    (cond
      (= :fixed mode)
      (if fixed?
        {:mode :fixed :width width :height height}
        {:mode :container})

      (= :container mode)
      (cond-> {:mode :container}
        width  (assoc :width width)
        height (assoc :height height))

      (some? mode) {:mode :container}
      fixed?       {:mode :fixed :width width :height height}
      :else        {:mode :container})))

(defn pending-window?
  "True when w normalizes to a :container window that still lacks a measured
   width or height."
  [w]
  (let [w (normalize-window w)]
    (and (= :container (:mode w))
         (or (nil? (:width w)) (nil? (:height w))))))

(defn ensure-window!
  "Write the normalized :window into plot-opts-a, but only when it changes.
   Call from a mount, never from render."
  [plot-opts-a]
  (let [opts (deref plot-opts-a)
        cur  (:window opts)
        next (normalize-window cur)]
    (when (not= cur next)
      (swap! plot-opts-a assoc :window next))
    plot-opts-a))

;; --- measured container component ------------------------------------------

(h/defc itera-container
  "Shadow-DOM scaffold that measures its own Hyper root and emits a bubbling
   \"resize\" event with {:width n :height n}.  `mode` only exists to satisfy
   defc's observed-attribute requirement and to keep the re-render boundary
   stable; it does not affect layout."
  [{:keys [mode]}]
  (render
   [:div {:style {:width "100%" :height "100%" :min-height "0"}} [:slot]])
  (mount [root]
    (let [measure
          (fn []
            (let [w (js/Math.round (.-clientWidth root))
                  h (js/Math.round (.-clientHeight root))]
              (when (and (> w 0) (> h 0))
                (let [last (.-last ctx)]
                  (when (or (not last)
                            (not= w (aget last 0))
                            (not= h (aget last 1)))
                    (set! (.-last ctx) (array w h))
                    (emit "resize" (js-obj "width" w "height" h)))))))
          schedule
          (fn []
            (when-let [t (.-timer ctx)]
              (js/clearTimeout t))
            (set! (.-timer ctx) (js/setTimeout measure 100)))]
      (set! (.-schedule ctx) schedule)
      (measure)
      (let [ro (new js/ResizeObserver (fn [_entries] (schedule)))]
        (set! (.-ro ctx) ro)
        (.observe ro root))))
  (update [_root]
    ;; Attribute changes (mode) must not replace the scaffold the observer
    ;; targets, but a mode switch can mean the measured dimensions are wanted
    ;; again: forget the last measurement and re-measure once it settles.
    (set! (.-last ctx) nil)
    (when-let [schedule (.-schedule ctx)]
      (schedule)))
  (unmount [_root]
    (when-let [t (.-timer ctx)]
      (js/clearTimeout t))
    (when-let [ro (.-ro ctx)]
      (.disconnect ro))
    (set! (.-timer ctx) nil)
    (set! (.-ro ctx) nil)
    (set! (.-last ctx) nil)
    (set! (.-schedule ctx) nil)))

;; --- render wrapper ---------------------------------------------------------

(defn- detail-number [detail k]
  (or (get detail k) (get detail (name k))))

(defn draw-container
  "Render `children` inside a measured container bound to plot-opts-a.

   In :container mode the component's debounced resize events write the
   measured width/height into plot-opts-a at [:window]; :fixed mode ignores
   those events and keeps the caller's dimensions."
  [plot-opts-a & children]
  (let [window (normalize-window (:window (deref plot-opts-a)))]
    (apply itera-container
           {:mode (name (:mode window))
            :style {:width "100%" :height "100%"
                    :min-width "0" :min-height "0"}
            :data-on:resize
            (h/action
             (swap! plot-opts-a
                    update :window
                    (fn [w]
                      (let [w      (normalize-window w)
                            width  (detail-number $detail :width)
                            height (detail-number $detail :height)]
                        (if (and (= :container (:mode w))
                                 (positive-integer? width)
                                 (positive-integer? height)
                                 (not= [width height]
                                       [(:width w) (:height w)]))
                          (assoc w :width width :height height)
                          w)))))}
           children)))