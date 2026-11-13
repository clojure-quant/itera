(ns demo.plot.misc.area.itera)

(defn calc-area [_ctx {:keys [a b]}]
  {:a a 
   :b b 
   :c (* a b)})

(defn options [_ctx]
  ; same format as in https://github.com/pink-gorilla/options
  [{:type :select :path [:a], :name "a", :spec [1 2 3 4 5 6 7 8 9 10]}
   {:type :select :path [:b], :name "b" :spec [1 2 3 4 5 6 7 8 9 10]}])


(defn plot [_opts {:keys [a b c]}]
  [:div
   [:div
    [:h1
     "Area Demo"]
    [:p "a:" (str a)]
    [:p "b:" (str b)]
    [:p "area:" (str c)]]])

(def algo-area
  {:calculate/options options
   :calculate/fn calc-area
   :calculate/option-initial {:a 1 :b 2}
   :plot/options {}
   :plot/fn plot})

