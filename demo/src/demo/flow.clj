(ns demo.flow
  (:require
   [missionary.core :as m]
   [tick.core :as t]))

(def dt-flow
  (let [f (m/ap
           (loop [dt (t/instant)]
             (m/amb
              dt
              (do (m/? (m/sleep 1000))
                  (recur (t/instant))))))]
    (with-meta f {:itera/output :flow})))

(defn local-time
  [i]
  (t/in i (t/zone)))

(defn EST-time
  [i]
  (t/in i (t/zone "America/New_York")))


(comment

  (-> dt-flow meta :itera/output)
  (-> [1 2 3] meta :itera/output)
  (m/? (m/reduce conj [] (m/eduction (take 3) dt-flow)))
  ;;=> [#time/instant "2026-11-17T02:18:13.669013043Z"
  ;;    #time/instant "2026-11-17T02:18:14.669443833Z"
  ;;    #time/instant "2026-11-17T02:18:15.669773496Z"]
  )