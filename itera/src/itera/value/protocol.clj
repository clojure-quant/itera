(ns itera.value.protocol)

(defprotocol itera-convertable
  (to-hiccup [v]))