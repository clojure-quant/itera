(ns itera.value.vector-table-info-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [itera.value.protocol :as protocol]
   [itera.value.vector-table-info :as table]))

(def ^:private rows
  [{:date "2024-01-01" :close 1.5}
   {:date "2024-01-02" :close 2.5}
   {:date "2024-01-03" :close 3.5}])

;; ---------------------------------------------------------------------------
;; vector support
;; ---------------------------------------------------------------------------

(deftest vector-total-rows-test
  (is (= 3 (protocol/total-rows rows)))
  (is (= 0 (protocol/total-rows []))))

(deftest vector-as-seq-of-maps-test
  (is (= rows (protocol/as-seq-of-maps rows))))

(deftest vector-select-rows-test
  (testing "a middle slice"
    (is (= [(nth rows 1)] (protocol/select-rows rows 1 2))))
  (testing "out-of-range bounds are clamped"
    (is (= rows (protocol/select-rows rows -5 100)))
    (is (= [] (protocol/select-rows rows 5 9))))
  (testing "an inverted range is empty"
    (is (= [] (protocol/select-rows rows 2 1)))))

(deftest vector-column-info-test
  (is (= {:date :string :close :number}
         (protocol/column-info rows))))

;; ---------------------------------------------------------------------------
;; seq support
;; ---------------------------------------------------------------------------

(deftest seq-total-rows-test
  (is (= 3 (protocol/total-rows (map identity rows))))
  (is (= 3 (protocol/total-rows (apply list rows))))
  (is (= 5 (protocol/total-rows (range 5)))))

(deftest seq-as-seq-of-maps-test
  (is (= rows (protocol/as-seq-of-maps (map identity rows)))))

(deftest seq-select-rows-test
  (testing "a seq slice is realized into a vector"
    (is (= [1 2] (protocol/select-rows (range 5) 1 3))))
  (testing "out-of-range bounds are clamped"
    (is (= [0 1 2] (protocol/select-rows (range 3) -5 100)))))

(deftest seq-column-info-test
  (is (= {:date :string :close :number}
         (protocol/column-info (map identity rows)))))

;; ---------------------------------------------------------------------------
;; public helpers
;; ---------------------------------------------------------------------------

(deftest data-columns-test
  (testing "columns are collected in first-seen order"
    (is (= [:a :b :c]
           (table/data-columns [{:a 1} {:b 2} {:c 3 :a 4}])))))

(deftest column-info-types-test
  (testing "numeric columns (integers and floats)"
    (is (= {:a :number} (table/column-info [{:a 1} {:a 2.5}]))))
  (testing "string column"
    (is (= {:a :string} (table/column-info [{:a "x"} {:a "y"}]))))
  (testing "boolean column"
    (is (= {:a :boolean} (table/column-info [{:a true} {:a false}]))))
  (testing "keyword column"
    (is (= {:a :keyword} (table/column-info [{:a :x} {:a :y}]))))
  (testing "java.time values are :time"
    (is (= {:d :time}
           (table/column-info [{:d (java.time.LocalDate/of 2024 1 1)}
                               {:d (java.time.LocalDate/of 2024 1 2)}])))
    (is (= {:d :time}
           (table/column-info [{:d (java.util.Date.)}]))))
  (testing "an all-nil column is :nil"
    (is (= {:a :nil} (table/column-info [{:a nil} {:a nil}]))))
  (testing "nil values are ignored when a column has other kinds"
    (is (= {:a :number} (table/column-info [{:a nil} {:a 1}]))))
  (testing "two distinct kinds are :mixed"
    (is (= {:a :mixed} (table/column-info [{:a 1} {:a "x"}]))))
  (testing "unclassified values are :unknown"
    (is (= {:a :unknown} (table/column-info [{:a {:b 1}}]))))
  (testing "empty data has no columns"
    (is (= {} (table/column-info [])))
    (is (= {} (table/column-info nil)))))