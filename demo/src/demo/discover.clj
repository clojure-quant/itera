(ns demo.discover
  (:require [clojure.pprint :as pprint]
            [itera.algo.discover :as discover]))

(defn -main [& _]
  (pprint/pprint (discover/discover)))
