(ns itera.plot-discover-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [itera.plot-discover :as pd]))

(def static-head
  [[:meta {:name "static"}]])

(def single-head
  [:link {:rel "stylesheet" :href "/single.css"}])

(def nil-head
  nil)

(defn dynamic-head
  [_req]
  [[:link {:rel "stylesheet" :href "/dynamic.css"}]])

(defn dynamic-single-head
  [_req]
  [:link {:rel "stylesheet" :href "/dynamic-single.css"}])

(deftest extension-requires-test
  (testing "combines requires of all extensions together"
    (is (= '[[a :refer [x]] [b :refer [y]] [c :refer [z]]]
           (pd/extension-requires [{:requires '[[a :refer [x]] [b :refer [y]]]}
                                   {:requires '[[c :refer [z]]]}]))))
  (testing "deduplicates across extensions"
    (is (= '[[a :refer [x]] [b :refer [y]]]
           (pd/extension-requires [{:requires '[[a :refer [x]]]}
                                   {:requires '[[a :refer [x]] [b :refer [y]]]}]))))
  (testing "tolerates extensions without :requires"
    (is (= '[[a :refer [x]]]
           (pd/extension-requires [{:requires '[[a :refer [x]]]} {}])))))

(deftest discover-reads-requires-test
  (is (some #(= '[itera.plot.hiccup :refer [hiccup]] %)
            (:requires (pd/discover)))))

(deftest resolve-heads-static-test
  (testing "static values flatten, and single elements are normalized"
    (is (= [[:meta {:name "static"}]
            [:link {:rel "stylesheet" :href "/single.css"}]]
           (pd/resolve-heads ['itera.plot-discover-test/static-head
                              'itera.plot-discover-test/single-head]))))
  (testing "nil values contribute nothing"
    (is (= [[:meta {:name "static"}]]
           (pd/resolve-heads ['itera.plot-discover-test/static-head
                              'itera.plot-discover-test/nil-head])))))

(deftest resolve-heads-dynamic-test
  (testing "a mixed static/dynamic head returns a function of the request"
    (let [head (pd/resolve-heads ['itera.plot-discover-test/static-head
                                  'itera.plot-discover-test/dynamic-head])]
      (is (fn? head))
      (is (= [[:meta {:name "static"}]
              [:link {:rel "stylesheet" :href "/dynamic.css"}]]
             (head {:hyper/route :fake})))))
  (testing "a dynamic head returning a single element is normalized"
    (let [head (pd/resolve-heads ['itera.plot-discover-test/dynamic-single-head])]
      (is (= [[:link {:rel "stylesheet" :href "/dynamic-single.css"}]]
             (head {}))))))
