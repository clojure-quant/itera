(ns itera.plot.rtable-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [itera.plot.rtable :as rtable]
   [itera.value.protocol :as spec-plot]))

(defn- rows
  [n]
  (mapv (fn [i] {:a i}) (range n)))

(def ^:private spec {:row-height "20px" :columns []})

(def ^:private window {:width 600 :height 232})

(defn- element
  "First child element of HICCUP with TAG, ignoring the attribute map."
  [tag hiccup]
  (first (filter #(and (vector? %) (= tag (first %))) (rest hiccup))))

(defn- body-rows
  "The :tr children of the :tbody in a rendered table HICCUP."
  [hiccup]
  (rest (element :tbody hiccup)))

(defn- paged [v pg]
  (spec-plot/to-formatted-paged-hiccup v spec window pg))

(deftest init-page-test
  (testing "25 rows at ten per page is three pages"
    (is (= {:page/row-size 10 :page/total 3 :page/current 1}
           (spec-plot/init-page (rtable/rtable (rows 25)) spec window))))
  (testing "fewer than one page is still one page"
    (is (= 1 (:page/total (spec-plot/init-page (rtable/rtable (rows 6)) spec window)))))
  (testing "an empty table is one empty page"
    (is (= 1 (:page/total (spec-plot/init-page (rtable/rtable []) spec window)))))
  (testing "no window height does not paginate"
    (is (= {:page/row-size nil :page/total 1 :page/current 1}
           (spec-plot/init-page (rtable/rtable (rows 25)) spec nil))))
  (testing "a window below the header height still fits one row"
    (is (= 1 (:page/row-size
              (spec-plot/init-page (rtable/rtable (rows 25)) spec
                                   {:width 600 :height 20}))))))

(deftest css-length-test
  (testing "px, rem, em and unitless lengths parse to pixels"
    (is (= 20.0 (rtable/css-length->px "20px")))
    (is (= 28.8 (rtable/css-length->px "1.8rem")))
    (is (= 32.0 (rtable/css-length->px "2em")))
    (is (= 40.0 (rtable/css-length->px "40"))))
  (testing "a non-length parses to nil"
    (is (nil? (rtable/css-length->px "abc")))))

(deftest rows-per-page-test
  (testing "a 400px window with 1.8rem rows fits twelve"
    (is (= 12 (rtable/rows-per-page {:row-height "1.8rem" :columns []}
                                    {:width 600 :height 400}))))
  (testing "no height does not paginate"
    (is (nil? (rtable/rows-per-page spec nil)))
    (is (nil? (rtable/rows-per-page spec {:width 600 :height 0})))))

(deftest paged-rows-test
  (let [v (rtable/rtable (rows 25))]
    (testing "the first page has ten rows"
      (is (= 10 (count (body-rows (paged v {:page/total 3 :page/current 1}))))))
    (testing "the last page has the remaining five rows"
      (is (= 5 (count (body-rows (paged v {:page/total 3 :page/current 3}))))))
    (testing "a current page above the last clamps instead of throwing"
      (is (= 5 (count (body-rows (paged v {:page/total 3 :page/current 99}))))))))

(deftest dynamic-geometry-test
  (let [v        (rtable/rtable (rows 25))
        window-5 {:width 600 :height 132}]
    (testing "a stale row-size is ignored in favour of the window geometry"
      (is (= 5 (count (body-rows
                       (spec-plot/to-formatted-paged-hiccup
                        v spec window-5 {:page/row-size 10
                                         :page/total 3
                                         :page/current 1}))))))
    (testing "no window height renders every row"
      (is (= 25 (count (body-rows
                        (spec-plot/to-formatted-paged-hiccup
                         v spec nil {:page/total 1 :page/current 1}))))))))