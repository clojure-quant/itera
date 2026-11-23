(ns itera.model-ui.container
  "Resizable, measured plot box for one interact-tab cell.

   Owns the dotted, user-resizable outer element for a cell and binds the
   resize action that writes the measured size back into a per-cell window
   cursor.  The heavy lifting (ResizeObserver, debounce, \"resize\" event) is
   itera.container/itera-container; this namespace only supplies the sizing
   style, the native resize handle, and the action that updates `window-a`.

   `window-a` is an atom-like cursor holding {:width <px> :height <px>}.  A
   user drag changes the element's size, the measured inner component reports
   it, and the action rewrites the cursor so the cell re-renders its plot at
   the new size."
  (:require [hyper.core :as h]
            [itera.container :as container]))

(defn- detail-number
  "Read k (a keyword, or its string name) from a $detail map."
  [detail k]
  (or (get detail k) (get detail (name k))))

(defn- positive-int
  "Round x to a positive integer, or nil when x is not a positive number.

   The measured values arrive as JSON numbers (long or double on the server),
   so accept any number rather than only integer? and normalize to a long."
  [x]
  (when (number? x)
    (let [n (long (Math/round (double x)))]
      (when (pos? n) n))))

(defn- resize-window!
  "Write the measured width/height from a \"resize\" $detail into window-a.

   Ignores events whose dimensions are missing, non-numeric, or not positive,
   and no-ops when the size is unchanged so a measurement never re-triggers a
   render."
  [window-a detail]
  (let [width  (positive-int (detail-number detail :width))
        height (positive-int (detail-number detail :height))]
    (when (and width height
               (not= [width height] [(:width @window-a) (:height @window-a)]))
      (swap! window-a assoc :width width :height height))))

(defn container
  "Dotted, user-resizable plot box bound to `window-a`.

   `window-a` is an atom-like cursor holding {:width <px> :height <px>}; the
   outer element carries the native CSS resize handle and the measured inner
   component reports size changes that update `window-a`.  `children` are
   projected into the box.

   `overflow` is hidden rather than auto: native resize only needs a
   non-visible overflow, and a scrollbar would shrink the measured content box
   by ~15px, feeding back into `window-a` as a runaway shrink."
  [window-a & children]
  (let [{:keys [width height]} @window-a]
    (apply container/itera-container
           {:mode "model-ui"
            :style {:width (str width "px")
                    :height (str height "px")
                    :min-width "0" :min-height "0"
                    :overflow "hidden"
                    :resize "both"
                    :border "dotted" :border-color "red"}
            :data-on:resize (h/action (resize-window! window-a $detail))}
           children)))
