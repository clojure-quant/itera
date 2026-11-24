(ns itera.plot.pixi-timeseries.pane-layout
  "Server-side pane geometry for the pixi-timeseries viewer.

   A spec is the canonical map

     {:panes [<pane> ...]
      :main-pane-min-height <px>
      :aux-pane-height <px>}

   where pane 0 is the main pane and panes 1..N are aux panes.  The viewer
   draws inside a fixed :window height; this namespace decides how many aux
   panes fit without making the main pane shorter than
   :main-pane-min-height.

   With

     h   = (:height window)
     m   = :main-pane-min-height (default `default-main-pane-min-height`)
     a   = :aux-pane-height (default `default-aux-pane-height`)
     aux = (max 0 (dec pane-count))

   the layout is

     k         = (min aux (max 0 (quot (- h m) a)))
     max-start = (max 0 (- aux k))
     start     = (clamp 0 max-start aux-start)
     main      = (- h (* k a))

   so the main pane fills the height left over after the visible aux panes.
   The visible panes are the contiguous run [0 start ... start+k-1]; the first
   visible aux pane is pane start+1.  `:overflow?` is true when some aux panes
   do not fit; the page editor then offers a selection in [0 max-start].

   Edge cases:

   - h is not a positive integer (a :container window before measurement):
     every pane is listed, `:heights` is nil, and `:overflow?` is false.  The
     client falls back to its legacy ratio split until the window is measured.
   - h < m: no aux pane fits (k is 0), so the main pane takes all of h; the
     minimum cannot be honoured at that size.  `:overflow?` may still be true.
   - k = 0 with aux panes: only the main pane is visible.  The combobox is
     still rendered because aux panes are undisplayed, but choosing one has no
     visible effect until the container is taller.
   - pane-count = 0: `:indices` and `:heights` are empty.

   This is a leaf namespace: it must not depend on `plot` or
   `plot-page-editor` (both of which require it) to avoid a require cycle.")

(def default-main-pane-min-height
  "Default minimum main pane height in pixels when the spec omits or misstates
   :main-pane-min-height."
  200)

(def default-aux-pane-height
  "Default aux pane height in pixels when the spec omits or misstates
   :aux-pane-height."
  100)

(defn positive-int?
  "True for a positive integer."
  [x]
  (and (integer? x) (pos? x)))

(defn- panes
  "The pane vector of SPEC, accepting the legacy bare vector shape."
  [spec]
  (if (map? spec) (:panes spec) spec))

(defn normalize-params
  "The effective geometry parameters of SPEC:
   {:main-pane-min-height <positive int> :aux-pane-height <positive int>}.

   Uses the spec keys when they are positive integers and falls back to
   `default-main-pane-min-height` / `default-aux-pane-height` otherwise."
  [spec]
  {:main-pane-min-height
   (let [m (:main-pane-min-height spec)]
     (if (positive-int? m) m default-main-pane-min-height))
   :aux-pane-height
   (let [a (:aux-pane-height spec)]
     (if (positive-int? a) a default-aux-pane-height))})

(defn- clamp [lo hi v]
  (max lo (min hi v)))

(defn- aux-start-index
  "AUX-START clamped to an integer >= 0 (invalid input counts as 0)."
  [v]
  (if (number? v)
    (max 0 (long v))
    0))

(defn layout
  "Lay out the panes of SPEC inside WINDOW at AUX-START.

   Returns

     {:main-height <px>            ; nil when the window height is invalid
      :aux-height <px>
      :main-pane-min-height <px>
      :aux-count <n>               ; max 0 (dec pane-count)
      :visible-aux-count <k>
      :start <clamped aux index>
      :max-start <n>
      :indices [0 start ...]       ; indices into :panes, main first
      :heights [<main> <aux> ...]  ; nil when the window height is invalid
      :overflow? (< k aux-count)
      :total-height <px>}

   See the namespace docstring for the formula, clamping and edge cases.
   SPEC may still be the legacy bare pane vector."
  [spec window aux-start]
  (let [pane-count (count (panes spec))
        {:keys [main-pane-min-height aux-pane-height]}
        (normalize-params spec)
        base {:main-pane-min-height main-pane-min-height
              :aux-height aux-pane-height}
        aux (max 0 (dec pane-count))
        h (:height window)]
    (if (positive-int? h)
      (let [k (min aux (max 0 (quot (- h main-pane-min-height)
                                     aux-pane-height)))
            max-start (max 0 (- aux k))
            start (clamp 0 max-start (aux-start-index aux-start))
            main-height (- h (* k aux-pane-height))
            indices (if (pos? pane-count)
                      (into [0] (map inc (range start (+ start k))))
                      [])
            heights (if (pos? pane-count)
                      (into [main-height] (repeat k aux-pane-height))
                      [])]
        (assoc base
               :main-height main-height
               :aux-count aux
               :visible-aux-count k
               :start start
               :max-start max-start
               :indices indices
               :heights heights
               :overflow? (< k aux)
               :total-height h))
      (assoc base
             :main-height nil
             :aux-count aux
             :visible-aux-count aux
             :start 0
             :max-start 0
             :indices (vec (range pane-count))
             :heights nil
             :overflow? false
             :total-height nil))))