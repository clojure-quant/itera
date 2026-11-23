(ns itera.model-ui.container-test
  (:require [clojure.test :refer [deftest is testing]]
            [itera.model-ui.container :as container]))

(def ^:private resize-window! #'container/resize-window!)

(deftest resize-window-writes-positive-dimensions-test
  (testing "keyword-keyed detail"
    (let [window-a (atom {:width 600 :height 400})]
      (resize-window! window-a {:width 700 :height 500})
      (is (= {:width 700 :height 500} @window-a))))
  (testing "string-keyed detail (name fallback)"
    (let [window-a (atom {:width 600 :height 400})]
      (resize-window! window-a {"width" 700 "height" 500})
      (is (= {:width 700 :height 500} @window-a))))
  (testing "numeric sizes are rounded to integers"
    (let [window-a (atom {:width 600 :height 400})]
      (resize-window! window-a {:width 700.6 :height 499.4})
      (is (= {:width 701 :height 499} @window-a)))))

(deftest resize-window-ignores-invalid-dimensions-test
  (doseq [[label detail] [["zero" {:width 0 :height 400}]
                          ["negative" {:width -1 :height 400}]
                          ["missing height" {:width 700}]
                          ["missing width" {:height 500}]
                          ["non-numeric width" {:width "700" :height 400}]
                          ["nil height" {:width 700 :height nil}]
                          ["empty detail" {}]
                          ["nil detail" nil]]]
    (testing label
      (let [window-a (atom {:width 600 :height 400})]
        (resize-window! window-a detail)
        (is (= {:width 600 :height 400} @window-a))))))

(deftest resize-window-noop-when-unchanged-test
  (let [window-a (atom {:width 600 :height 400})
        writes   (atom 0)]
    (add-watch window-a :count (fn [_ _ _ _] (swap! writes inc)))
    (resize-window! window-a {:width 600 :height 400})
    (is (zero? @writes))
    (resize-window! window-a {:width 700 :height 400})
    (is (= 1 @writes))
    (is (= {:width 700 :height 400} @window-a))))
