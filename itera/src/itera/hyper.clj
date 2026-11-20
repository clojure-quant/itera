(ns itera.hyper
  (:require [hyper.core :as h]
            [itera.algo.discover :as discover]
            [itera.plot-discover :as plot-discover]
            [itera.routes :as routes]))

(defn start
  "Start a Hyper server for the itera routes.
   Takes a map {:keys [ctx opts]}:
     ctx  - application context map (passed to itera.routes/routes); its
            :requires are merged with the :requires declared by all
            `itera-plots.edn` extensions, so workbooks can call their plot
            functions without the host repeating them
     opts - Hyper opts map, e.g. {:port 3000 :head [...] :static-resources \"public\"};
          when :head is absent it defaults to the heads declared under :head in
          all `itera-plots.edn` resources on the classpath (see
          itera.plot-discover).  Each of those resources is a map
          {:head [<qualified symbols>] :option-ui [<namespace symbols>]
           :requires [<libspecs>]}; the :option-ui namespaces are always
          required, registering their option editors.  Head declarations may
          be static hiccup or functions of the request returning hiccup; if
          any is dynamic the default head becomes a function too.
   Extra optional key:
     :routes - extra reitit route entries appended after the discovered itera
               routes (see itera.routes/routes).

   Starts the discovery watcher, then builds the routes from its state.
   Returns a state map to pass to stop."
  [{:keys [ctx opts] :as m}]
  (let [{:keys [head option-ui requires]} (plot-discover/discover)
        _ (plot-discover/require-option-uis! option-ui)
        ctx (update ctx :requires #(vec (distinct (concat requires %))))
        opts (update opts :head #(or % (plot-discover/resolve-heads head)))
        discovery (discover/discover-start)
        itera-routes (routes/routes ctx discovery)
        all-routes (into (vec itera-routes) (:routes m))
        ;; create-handler takes kwargs; forward the Hyper-creation keys.
        handler (apply h/create-handler
                       all-routes
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
