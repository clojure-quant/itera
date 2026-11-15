(ns itera.hyper
  (:require [hyper.core :as h]
            [itera.algo.discover :as discover]
            [itera.plot-discover :as plot-discover]
            [itera.routes :as routes]))

(defn start
  "Start a Hyper server for the itera routes.
   Takes a map {:keys [ctx opts]}:
     ctx  - application context map (passed to itera.routes/routes)
     opts - Hyper opts map, e.g. {:port 3000 :head [...] :static-resources \"public\"};
          when :head is absent it defaults to the heads declared under :head in
          all `itera-plots.edn` resources on the classpath (see
          itera.plot-discover).  Each of those resources is a map
          {:head [<qualified symbols>] :option-ui [<namespace symbols>]}; the
          :option-ui namespaces are always required, registering their option
          editors.
   Starts the discovery watcher, then builds the routes from its state.
   Returns a state map to pass to stop."
  [{:keys [ctx opts]}]
  (let [{:keys [head option-ui]} (plot-discover/discover)
        _ (plot-discover/require-option-uis! option-ui)
        opts (update opts :head #(or % (plot-discover/resolve-heads head)))
        discovery (discover/discover-start)
        routes (routes/routes ctx discovery)
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
     :discover discovery
     :stop stop}))

(defn stop
  "Stop the Hyper server started by `start` and tear down the discover watcher."
  [state]
  (when-let [stop (:stop state)] (stop))
  (when-let [d (:discover state)] (discover/discover-end d))
  nil)
