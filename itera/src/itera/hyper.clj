(ns itera.hyper
  (:require [hyper.core :as h]
            [itera.discover :as discover]
            [itera.routes :as routes]))

(defn start
  "Start a Hyper server for the itera routes.
   Takes a map {:keys [ctx opts]}:
     ctx  - application context map (passed to itera.routes/routes)
     opts - Hyper opts map, e.g. {:port 3000 :head [...] :static-resources \"public\"}
   Returns a state map to pass to stop."
  [{:keys [ctx opts]}]
  (let [{:keys [routes discover]} (routes/routes ctx)
        ;; create-handler takes kwargs; forward the Hyper-creation keys.
        handler (apply h/create-handler
                       routes
                       (mapcat identity (select-keys opts
                                                     [:head :static-resources :static-dir
                                                      :watches :middleware :render-middleware
                                                      :render-error :base-path :squint-core-url
                                                      :datastar-script])))
        stop (h/start! handler {:port (:port opts 3000)})]
    {:ctx ctx
     :opts opts
     :handler handler
     :discover discover
     :stop stop}))

(defn stop
  "Stop the Hyper server started by `start` and tear down the discover watcher."
  [state]
  (when-let [stop (:stop state)] (stop))
  (when-let [d (:discover state)] (discover/discover-end d))
  nil)
