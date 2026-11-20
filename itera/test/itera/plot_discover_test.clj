(ns itera.plot-discover-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [itera.plot-discover :as pd]))

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
