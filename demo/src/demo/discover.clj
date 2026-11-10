(ns demo.discover
  (:require [clojure.pprint :as pprint]
            [itera.discover :as discover]))

(defn -main [& _]
  (pprint/pprint (discover/discover)))
