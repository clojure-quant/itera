(ns itera.page.algo
  (:require [hyper.core :as h]
            [itera.algo :as algo]
            [itera.container :as container]
            [itera.options :as options]))

(defn- find-algo
  "Find the discovery entry whose :name matches name."
  [algos name]
  (some #(when (= name (:name %)) %) algos))

(defn- algo-map
  "Resolve a discovery entry's :algo (a Var) to its value."
  [entry]
  (let [v (:algo entry)]
    (if (var? v) (var-get v) v)))

(defn algo-page
  "Form-3 view for one algo: owns the calculate/plot lifecycle across
   mount/unmount over tab-scoped cursors."
  [ctx result*]
  (fn [req]
    (let [{n :ns s :symbol} (get-in req [:hyper/route :path-params])
          name (str n "/" s)]
      (if-let [entry (find-algo (:algos @result*) name)]
        (let [m    (algo-map entry)
              base [:itera.algo name]]
          (h/view
           {:mount
            (fn []
              (let [calc-a (h/tab-cursor (conj base :calculate-options)
                                         (:calculate/option-initial m {}))
                    plot-a (h/tab-cursor (conj base :plot-options)
                                         (:plot/option-initial m {}))
                    res-a  (h/tab-cursor (conj base :result))]
                (container/ensure-window! plot-a)
                (algo/init-calculate calc-a plot-a res-a m)
                {:calc-a calc-a :plot-a plot-a :res-a res-a :m m
                 :calc-spec (options/resolve-spec (:calculate/options m) ctx)
                 :plot-spec (options/resolve-spec (:plot/options m) ctx)
                 :watch (algo/start-calculate ctx calc-a plot-a res-a m)}))
            :render
            (fn [r _req]
              (let [plot-opts @(:plot-a r)]
                [:div.page
                 [:div.algo-layout
                  {:style {:display "flex" :gap "1.5rem" :align-items "flex-start"}}
                  [:div.algo-options {:style {:flex "0 0 280px"
                                              :min-width "240px"
                                              :max-height "calc(100vh - 1rem)"
                                              :overflow-y "auto"
                                              :position "sticky"
                                              :top "0"
                                              :padding-right "0.5rem"}}
                   (when (seq (:calc-spec r))
                     (options/options-ui {:group :calculate :spec (:calc-spec r)
                                          :value @(:calc-a r)
                                          :set-fn (fn [p v] (swap! (:calc-a r) assoc-in p v))
                                          :update-fn (fn [p f] (swap! (:calc-a r) update-in p f))}))
                   (when (seq (:plot-spec r))
                     (options/options-ui {:group :plot :spec (:plot-spec r)
                                          :value plot-opts
                                          :set-fn (fn [p v] (swap! (:plot-a r) assoc-in p v))
                                          :update-fn (fn [p f] (swap! (:plot-a r) update-in p f))}))]
                  [:div.algo-result {:style {:flex "1" :min-width "0" :min-height "0"
                                             :height "calc(100vh - 1rem)"
                                             :display "flex"}}
                   (container/draw-container
                    (:plot-a r)
                    (if (container/pending-window? (:window plot-opts))
                      [:div {:style {:width "100%" :height "100%"}}]
                      (or (algo/plot-result (:plot-a r) (:res-a r) (:m r))
                          [:div])))]]]))
            :unmount
            (fn [r] (algo/stop-calculate (:watch r)))}))
        [:div.page [:h1 "Algo not found"] [:p name]]))))
