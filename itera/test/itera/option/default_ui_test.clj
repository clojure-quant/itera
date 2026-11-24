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