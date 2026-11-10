(ns itera.core)


(defprotocol algo
  (calculate-options [ctx])
  (calculate [ctx options])
  (draw-options [ctx])
  (draw [ctx result options]))

