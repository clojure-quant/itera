(ns itera.routes
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [itera.algo :as algo]
            [itera.discover :as discover]
            [itera.options :as options]))

(defn- split-name
  "Split an alg name \"<ns>/<symbol>\" into {:ns <ns> :symbol <symbol>}."
  [name]
  (let [[n s] (str/split name #"/" 2)]
    {:ns n :symbol s}))

(defn- overview-page
  "Form-2 view: subscribes to the discover result atom so the page re-renders
   when itera.clj files change."
  [result*]
  (fn [req]
    (h/watch! result*)
    (fn [req]
      (let [{:keys [algos error]} @result*]
        [:div.page
         [:h1 "Itera"]
         [:ul
          (for [{:keys [name]} algos]
            [:li {:key name}
             [:a (h/navigate :itera-algo (split-name name)) name]])]
         (when (seq error)
           [:div.errors
            [:h2 "Errors"]
            [:table
             [:thead [:tr [:th "file"] [:th "error"]]]
             [:tbody
              (for [{:keys [file error]} error]
                [:tr {:key file} [:td file] [:td (str error)]])]]])]))))

(defn- find-algo
  "Find the discovery entry whose :name matches name."
  [algos name]
  (some #(when (= name (:name %)) %) algos))

(defn- algo-map
  "Resolve a discovery entry's :algo (a Var) to its value."
  [entry]
  (let [v (:algo entry)]
    (if (var? v) (var-get v) v)))

(defn- algo-page
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
                (algo/init-calculate calc-a plot-a res-a m)
                {:calc-a calc-a :plot-a plot-a :res-a res-a :m m
                 :calc-spec (options/resolve-spec (:calculate/options m) ctx)
                 :plot-spec (options/resolve-spec (:plot/options m) ctx)
                 :watch (algo/start-calculate ctx calc-a plot-a res-a m)}))
            :render
            (fn [r _req]
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
                                        :value @(:plot-a r)
                                        :set-fn (fn [p v] (swap! (:plot-a r) assoc-in p v))
                                        :update-fn (fn [p f] (swap! (:plot-a r) update-in p f))}))]
                [:div.algo-result {:style {:flex "1" :min-width "0"}}
                 (or (algo/plot-result (:plot-a r) (:res-a r) (:m r)) [:div])]]])
            :unmount
            (fn [r] (algo/stop-calculate (:watch r)))}))
        [:div.page [:h1 "Algo not found"] [:p name]]))))

(defn routes
  "Build the itera routes. Calls discover-start once and returns
   {:routes <vector> :discover <discover-start state>}."
  [ctx]
  (let [d (discover/discover-start)
        result* (:result d)]
    {:routes
     [["/itera" {:name :itera
                 :title "Itera"
                 :get (overview-page result*)}]
      ["/itera/algo/:ns/:symbol" {:name  :itera-algo
                                  :title "Algo"
                                  :get   (algo-page ctx result*)}]]
     :discover d}))
