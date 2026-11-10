(ns options.itera)

(defn calculate [_ctx options]
  options)

(defn options [_ctx]
  [{:type :label   :name "Basic"}
   {:type :bool    :path [:bool]        :name "Bool"}
   {:type :string  :path [:string]      :name "String"}
   {:type :select  :path [:select]      :name "Select (values)" :spec [1 2 3 4 5]}
   {:type :select  :path [:select-map]  :name "Select (named)"
    :spec [{:id :apple  :name "Apple"}
           {:id :banana :name "Banana"}
           {:id :cherry :name "Cherry"}]}
   {:type :label   :name "Advanced"}
   {:type :select-multiple :path [:multi] :name "Multi" :spec [:a :b :c :d]}
   {:type :view    :path [:string]      :name "View"}])

(defn plot [_opts result]
  [:pre {:style {:margin 0 :padding "0.75rem" :background "#f6f6f6"
                 :borderRadius "0.375rem" :overflow "auto"}}
   (pr-str result)])

(def options-algo
  {:calculate/options options
   :calculate/fn calculate
   :calculate/option-initial {:bool false
                              :string "hello"
                              :select 2
                              :select-map :apple
                              :multi [:a :b]}
   :plot/options {}
   :plot/fn plot})
