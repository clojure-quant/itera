(ns itera.plot.pixi-timeseries.window
  "Shared row-window geometry for the pixi-timeseries viewer.

   The live window width and the selector :step are authoritative at read
   time: the number of rows a viewport can show is derived from them on every
   render, so a container resize (which changes :window :width) re-windows the
   series without any state write.  Stored :rows-visible is informational and
   used only as a fallback.

   This is a leaf namespace: it must not depend on `plot` or `plot-page-editor`
   (both of which require it) to avoid a require cycle.")

(def default-step
  "Pixels per row used when the selector has no positive :step."
  8)

(defn pixels-per-row
  "Positive pixels-per-row from SELECTOR's :step, defaulting to `default-step`
   for a missing/invalid step.  SELECTOR is the :window-selector map (or nil)."
  [selector]
  (let [s (:step selector)]
    (if (and (number? s) (pos? s)) s default-step)))

(defn rows-visible
  "Rows per viewport from WINDOW's :width and SELECTOR's :step, as
   `(max 1 (long (Math/floor (/ width step))))`.

   Returns nil when WINDOW's :width is not a positive number (e.g. a
   :container window before the first measurement); callers fall back to a
   stored row count in that case.  Does not clamp to a row count."
  [window selector]
  (let [w (:width window)]
    (when (and (number? w) (pos? w))
      (max 1 (long (Math/floor (/ (double w) (pixels-per-row selector))))))))
