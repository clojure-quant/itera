(ns demo.plot.table.rtable.itera
  (:require [demo.data :as data]
            [itera.plot.rtable :as rtable]))

(def simple-columns
  [{:path :close :header "Close" :format (fn [v] (str v))
    :style {:border "1px solid #ccc" :padding "4px 10px"}}
   {:path :sma :header "SMA" :format (fn [v] (str "#" v))
    :style {:border "1px solid #ccc" :padding "4px 10px"}}])

(def simple-rows
  [{:close 10.444 :sma 10}
   {:close 14 :sma 12}
   {:close 9 :sma 11}
   {:close 16 :sma 13}
   {:close 11 :sma 12}
   {:close 10 :sma 11}])

(def bars-columns
  [{:path :date :header "Date" :max-width "180px"
    :format (fn [v] (str v))}
   {:path :close :header "Close" :format (fn [v] (str v))}
   {:path :volume :header "Volume"}])

(defn bars-rows []
  (take 100 (data/bars)))

(def examples
  {:rtable/simple
   {:label "Simple"
    :columns simple-columns
    :data    (fn [] simple-rows)}

   :rtable/bars
   {:label "Bars"
    :columns bars-columns
    :data    bars-rows}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :rtable/simple}}]
  (let [{:keys [columns data]} (get examples example)]
    {:example example :columns columns :data (data)}))

(defn plot [_opts {:keys [columns data]}]
  (rtable/rtable {:columns columns
                  :rows    data
                  :style   {:border-collapse "collapse"}}))

(def algo-rtable
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :rtable/simple}
   :plot/options {}
   :plot/fn plot})
