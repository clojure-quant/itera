(ns itera.algo)

(def ^:private calculate-watch-key ::calculate)

(defn init-calculate
  "Initialize the calculate lifecycle.  Option cursors are seeded by the
   caller via tab-cursor default-values (only when never written), so this
   only resets the derived result.  m is the discovered algo map."
  [_calculate-options-a _plot-options-a result-a _m]
  (reset! result-a nil))

(defn start-calculate
  "Watch calculate-options-a; on change run ((:calculate/fn m) ctx options)
   and store it in result-a.  Also runs once immediately.  Returns the state
   passed to stop-calculate."
  [ctx calculate-options-a plot-options-a result-a m]
  (let [calc (:calculate/fn m)
        run  (fn [options] (when calc (reset! result-a (calc ctx options))))]
    (add-watch calculate-options-a calculate-watch-key
               (fn [_k _ref _old new] (run new)))
    (run @calculate-options-a)
    {:watchable calculate-options-a :key calculate-watch-key}))

(defn stop-calculate
  "Remove the watch installed by start-calculate."
  [state]
  (when state
    (remove-watch (:watchable state) (:key state))))

(defn plot-result
  "Render via ((:plot/fn m) @plot-options-a @result-a)."
  [plot-options-a result-a m]
  (when-let [plot (:plot/fn m)]
    (plot @plot-options-a @result-a)))
