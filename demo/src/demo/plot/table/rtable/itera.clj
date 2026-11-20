(ns demo.plot.table.rtable.itera
  (:require [demo.data :as data]
            [itera.plot.rtable :as rtable]))

(def simple-rows
  [{:close 10.444 :sma 10}
   {:close 14 :sma 12}
   {:close 9 :sma 11}
   {:close 16 :sma 13}
   {:close 11 :sma 12}
   {:close 10 :sma 11}])

(defn bars-rows []
  (take 100 (data/bars)))

(defn- person
  "One generated person; the values mirror dali-table's data2.cljs."
  [^java.util.Random r i]
  {:id i
   :first-name "John"
   :last-name (str "Doe " i)
   :desc (str "Person " i " description")
   :age (* 100.0 (.nextDouble r))
   :sex (if (< (.nextDouble r) 0.5) :man :woman)
   :x (* 100.0 (.nextDouble r))
   :y (* 100.0 (.nextDouble r))
   :z (* 100.0 (.nextDouble r))
   :a (* 100.0 (.nextDouble r))
   :b (* 100.0 (.nextDouble r))
   :c (* 100.0 (.nextDouble r))
   :1 (* 100.0 (.nextDouble r))
   :2 (* 100.0 (.nextDouble r))
   :3 (* 100.0 (.nextDouble r))})

(defn persons
  "Seeded persons dataset, stable across reloads."
  []
  (let [r (java.util.Random. 42)]
    (mapv #(person r %) (range 1000))))

(def examples
  {:rtable/simple  {:label "Simple"  :data (fn [] simple-rows)}
   :rtable/bars    {:label "Bars"    :data bars-rows}
   :rtable/persons {:label "Persons" :data persons}})

(defn options [_ctx]
  [{:type :select
    :path [:example]
    :name "Example"
    :spec (mapv (fn [[id {:keys [label]}]]
                  {:id id :name label})
                examples)}])

(defn calculate [_ctx {:keys [example] :or {example :rtable/simple}}]
  (let [{:keys [data]} (get examples example)]
    {:example example :data (data)}))

(defn plot [_opts {:keys [data]}]
  (rtable/rtable data))

(def algo-rtable
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:example :rtable/simple}
   :plot/options {}
   :plot/fn plot})
