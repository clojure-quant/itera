(ns itera.option.default-ui-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [hyper.context :as context]
   [hyper.test :as ht]
   [itera.option.core :as options]
   [itera.option.default-ui]))

(def ^:private frequency-spec
  [{:id :m1 :name "1 minute"}
   {:id :m5 :name "5 minutes"}])

(defn- render-editor [opt value set-fn]
  (-> (ht/test-page (fn [_] (options/editor opt value set-fn nil nil)))
      :body))

(defn- render-option
  "Render a single option spec through options-ui; return the test-page result."
  [opt value set-fn]
  (ht/test-page
   (fn [_]
     (options/options-ui {:spec [opt] :value value :set-fn set-fn}))))

(deftest select-nil-value-renders-blank-option-test
  (let [body (render-editor {:type :select :spec frequency-spec} nil (fn [_ _]))
        opts (drop 2 body)]
    (testing "the first option is a selected blank placeholder"
      (is (= [:option {:value "" :selected true} ""] (first opts))))
    (testing "no real option is selected"
      (is (not-any? #(true? (get-in % [1 :selected])) (rest opts))))))

(deftest select-value-renders-no-blank-option-test
  (let [body (render-editor {:type :select :spec frequency-spec} :m5 (fn [_ _]))
        opts (drop 2 body)]
    (testing "no blank placeholder is rendered"
      (is (= 2 (count opts)))
      (is (not-any? #(= "" (second %)) opts)))
    (testing "the matching option is selected"
      (is (= [:option {:value ":m5" :selected true} "5 minutes"] (second opts))))))

(deftest select-multiple-namespaced-group-test
  (let [written (atom nil)
        set-fn  (fn [p v] (swap! written assoc-in p v))
        result  (ht/test-page
                 (fn [_]
                   (options/options-ui
                    {:group   :model/option-ui-values
                     :spec    [{:type :select-multiple
                                :name "symbols"
                                :path [:selected-symbols]
                                :spec ["AAPL" "MSFT" "GOOGL"]}]
                     :value   {:selected-symbols ["AAPL"]}
                     :set-fn  set-fn})))]
    (testing "the signal store path drops the keyword namespace"
      (is (contains? (:signals result)
                     [:itera-options :option-ui-values :selected-symbols])))
    (testing "the action reads client-reported ids from the plain path"
      (let [action (-> result :actions vals first)]
        (binding [context/*signals* {:itera-options
                                     {:option-ui-values
                                      {:selected-symbols ["1" "3"]}}}]
          ((:fn action) {}))
        (is (= {:selected-symbols ["AAPL" "GOOGL"]} @written))))))

(defn- editor-input
  "The attrs map of the single option's rendered :input node."
  [result]
  (get-in result [:body 2 2 1]))

(deftest int-editor-test
  (let [written (atom nil)
        set-fn  (fn [p v] (swap! written assoc-in p v))
        result  (render-option {:type :int :name "count" :path [:count]} nil set-fn)
        input   (editor-input result)
        action  (-> result :actions vals first)]
    (testing "renders a number input"
      (is (= "number" (:type input)))
      (is (= "" (:value input)))
      (is (= "count" (:placeholder input))))
    (testing "parses a numeric string into an Integer"
      ((:fn action) {:value "12"})
      (is (= {:count 12} @written)))
    (testing "a blank value writes nil"
      ((:fn action) {:value ""})
      (is (= {:count nil} @written)))
    (testing "a non-numeric value writes nil"
      ((:fn action) {:value "abc"})
      (is (= {:count nil} @written)))))

(deftest double-editor-test
  (let [written (atom nil)
        set-fn  (fn [p v] (swap! written assoc-in p v))
        result  (render-option {:type :double :name "ratio" :path [:ratio]} nil set-fn)
        input   (editor-input result)
        action  (-> result :actions vals first)]
    (testing "renders a number input"
      (is (= "number" (:type input)))
      (is (= "" (:value input)))
      (is (= "ratio" (:placeholder input))))
    (testing "parses a numeric string into a Double"
      ((:fn action) {:value "1.5"})
      (is (= {:ratio 1.5} @written)))
    (testing "a blank value writes nil"
      ((:fn action) {:value ""})
      (is (= {:ratio nil} @written)))
    (testing "a non-numeric value writes nil"
      ((:fn action) {:value "abc"})
      (is (= {:ratio nil} @written)))))