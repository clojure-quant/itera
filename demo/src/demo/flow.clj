(ns demo.flow
  (:require
   [missionary.core :as m]
   [tick.core :as t]))

(def dt-flow
  (->> (m/ap
        (loop [dt (t/instant)]
          (m/amb
           dt
           (do (m/? (m/sleep 1000))
               (recur (t/instant)))
           )))
       ))

(comment 
  
  (m/? (m/reduce conj [] (m/eduction (take 3) dt-flow)))
  ;;=> [#time/instant "2026-11-17T02:18:13.669013043Z"
  ;;    #time/instant "2026-11-17T02:18:14.669443833Z"
  ;;    #time/instant "2026-11-17T02:18:15.669773496Z"]
  
  
  
  )