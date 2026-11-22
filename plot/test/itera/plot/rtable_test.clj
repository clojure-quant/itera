(ns itera.plot.rtable-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [itera.plot.rtable :as rtable]
   [itera.value.protocol :as spec-plot]))

(defn- rows
  [n]
  (mapv (fn [i] {:a i}) (range n)))

(defn- element
  "First child element of HICCUP with TAG, ignoring the attribute map."
  [tag hiccup]
  (first (filter #(and (vector? %) (= tag (first %))) (rest hiccup))))

(defn- body-rows
  "The :tr children of the :tbody in a rendered table HICCUP."
  [hiccup]
  (rest (element :tbody hiccup)))

(deftest init-page-test
  (testing "25 rows is three pages of ten"
    (is (= {:page/row-size 10 :page/total 3 :page/current 1}
           (spec-plot/init-page (rtable/rtable (rows 25)) nil nil))))
  (testing "fewer than one page is still one page"
    (is (= 1 (:page/total (spec-plot/init-page (rtable/rtable (rows 6)) nil nil)))))
  (testing "an empty table is one empty page"
    (is (= 1 (:page/total (spec-plot/init-page (rtable/rtable []) nil nil))))))

(deftest paged-rows-test
  (let [v    (rtable/rtable (rows 25))
        spec (spec-plot/default-spec v)]
    (testing "the first page has ten rows"
      (is (= 10 (count (body-rows
                        (spec-plot/to-formatted-paged-hiccup
                         v spec nil {:page/row-size 10
                                     :page/total 3
                                     :page/current 1}))))))
    (testing "the last page has the remaining five rows"
      (is (= 5 (count (body-rows
                       (spec-plot/to-formatted-paged-hiccup
                        v spec nil {:page/row-size 10
                                    :page/total 3
                                    :page/current 3}))))))
    (testing "a current page above the last clamps instead of throwing"
      (is (= 5 (count (body-rows
                       (spec-plot/to-formatted-paged-hiccup
                        v spec nil {:page/row-size 10
                                    :page/total 3
                                    :page/current 99}))))))))