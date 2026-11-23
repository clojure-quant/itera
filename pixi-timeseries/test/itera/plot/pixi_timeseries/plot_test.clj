(ns itera.plot.pixi-timeseries.plot-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [itera.plot.pixi-timeseries.plot :as pixi]
   [itera.plot.pixi-timeseries.plot-format-ui :as fmt]
   [itera.value.protocol :as protocol]
   [tech.v3.dataset :as ds]))

(def ^:private window
  {:width 600 :height 400})

(defn- ohlc-rows
  [n]
  (mapv (fn [i]
          {:date (str (.plusDays (java.time.LocalDate/of 2024 1 1) i))
           :open (+ 100.0 i)
           :high (+ 102.0 i)
           :low  (+ 99.0 i)
           :close (+ 101.0 i)})
        (range n)))

;; ---------------------------------------------------------------------------
;; default spec
;; ---------------------------------------------------------------------------

(deftest default-spec-ohlc-test
  (is (= [[{:type :bar :col [:open :high :low :close]}]]
         (fmt/default-spec* (ohlc-rows 3)))))

(deftest default-spec-line-test
  (testing ":close is preferred for non-OHLC data"
    (is (= [[{:type :line :col :close}]]
           (fmt/default-spec* [{:date "2024-01-01" :close 1.5}]))))
  (testing "the first numeric non-time column is used"
    (is (= [[{:type :line :col :volume}]]
           (fmt/default-spec* [{:date "2024-01-01" :asset "a" :volume 3}]))))
  (testing "time and non-numeric columns are skipped"
    (is (= []
           (fmt/default-spec* [{:epoch 1 :date "2024-01-01"
                                :asset "a" :entry :flat}])))))

(deftest default-spec-empty-test
  (is (= [] (fmt/default-spec* [])))
  (is (= [] (fmt/default-spec* nil))))

(deftest default-spec-protocol-test
  (let [rows (ohlc-rows 3)]
    (is (= (fmt/default-spec* rows)
           (protocol/default-spec (pixi/pixi-timeseries rows))))))

;; ---------------------------------------------------------------------------
;; data-only record and rows-only constructor
;; ---------------------------------------------------------------------------

(deftest record-data-only-test
  (let [rows (ohlc-rows 3)
        v    (pixi/pixi-timeseries rows)]
    (is (= rows (:data v)))
    (is (= [:data] (keys v)))))

(deftest to-hiccup-default-test
  (let [h (protocol/to-hiccup (pixi/pixi-timeseries (ohlc-rows 3)))]
    (is (= :itera-pixi-timeseries (first h)))
    (is (= "900px" (get-in h [1 :style :width])))
    (is (= "600px" (get-in h [1 :style :height])))))

(deftest ds-constructor-test
  (is (= [{:close 1.0}]
         (:data (pixi/pixi-timeseries [{:close 1.0}])))))

;; ---------------------------------------------------------------------------
;; seq and dataset data (protocol-only storage)
;; ---------------------------------------------------------------------------

(deftest seq-data-test
  (let [rows (map identity (ohlc-rows 24))
        v    (pixi/pixi-timeseries rows)]
    (testing "the seq is stored as-is, not coerced to a vector"
      (is (= rows (:data v)))
      (is (instance? clojure.lang.LazySeq (:data v))))
    (testing "the default spec works from seq data"
      (is (= [[{:type :bar :col [:open :high :low :close]}]]
             (protocol/default-spec v))))
    (testing "the preformatted path works from seq data"
      (is (= :itera-pixi-timeseries
             (first (protocol/to-formatted-hiccup v nil window)))))
    (testing "paging works from seq data"
      (is (= {:display :nice
              :step 8
              :end 24
              :rows-all 24
              :rows-visible 24}
             (protocol/init-page v nil window)))
      (let [hiccup (protocol/to-formatted-paged-hiccup v nil window nil)]
        (is (= :itera-pixi-timeseries (first hiccup)))
        (is (str/includes? (:options (second hiccup)) "2024-01-24"))))))

(deftest dataset-data-test
  (let [d  (ds/->dataset
            {:date  (mapv #(str (.plusDays (java.time.LocalDate/of 2024 1 1) %))
                          (range 12))
             :open  (mapv #(+ 100.0 %) (range 12))
             :high  (mapv #(+ 102.0 %) (range 12))
             :low   (mapv #(+ 99.0 %) (range 12))
             :close (mapv #(+ 101.0 %) (range 12))})
        v  (pixi/pixi-timeseries d)]
    (testing "the dataset is stored as-is"
      (is (identical? d (:data v))))
    (testing "column types are inferred from the dataset datatypes"
      (is (= :number (:close (protocol/column-info (:data v)))))
      (is (= :string (:date (protocol/column-info (:data v))))))
    (testing "the default spec and paged path work from dataset data"
      (is (= [[{:type :bar :col [:open :high :low :close]}]]
             (protocol/default-spec v)))
      (let [hiccup (protocol/to-formatted-paged-hiccup v nil window nil)]
        (is (= :itera-pixi-timeseries (first hiccup)))
        (is (str/includes? (:options (second hiccup)) "2024-01-12"))))))

;; ---------------------------------------------------------------------------
;; normalize spec
;; ---------------------------------------------------------------------------

(deftest normalize-spec-test
  (testing "a charts vector is accepted and its keys preserved"
    (let [spec [[{:type :bar :col [:open :high :low :close] :extra true}]
                [{:type :line :col :close}]]]
      (is (= spec (fmt/normalize-spec spec)))))
  (testing "a non-vector pane becomes an empty pane"
    (is (= [[{:type :line :col :close}] []]
           (fmt/normalize-spec [[{:type :line :col :close}] {:oops 1}]))))
  (testing "an empty vector is valid"
    (is (= [] (fmt/normalize-spec []))))
  (testing "maps, nil and scalars are invalid"
    (is (nil? (fmt/normalize-spec {:charts []})))
    (is (nil? (fmt/normalize-spec nil)))
    (is (nil? (fmt/normalize-spec 42)))))

;; ---------------------------------------------------------------------------
;; page protocol
;; ---------------------------------------------------------------------------

(deftest init-page-test
  (let [v    (pixi/pixi-timeseries (ohlc-rows 240))
        page (protocol/init-page v nil window)]
    (is (= {:display :nice
            :step 8
            :end 240
            :rows-all 240
            :rows-visible 75}
           page))))

(deftest paged-hiccup-test
  (let [rows   (ohlc-rows 240)
        v      (pixi/pixi-timeseries rows)
        page   {:display :nice :step 8 :end 100 :rows-all 240 :rows-visible 75}
        hiccup (protocol/to-formatted-paged-hiccup v nil window page)
        slice  (pixi/select-rows {:window window :window-selector page} rows)]
    (is (= :itera-pixi-timeseries (first hiccup)))
    (testing "the page slice is the last 75 rows up to the exclusive :end"
      (is (= 75 (count slice)))
      (is (= "2024-01-26" (:date (first slice))))
      (is (= "2024-04-09" (:date (last slice)))))
    (testing "the payload carries only the page slice"
      (let [payload (:options (second hiccup))]
        (is (str/includes? payload "2024-01-26"))
        (is (str/includes? payload "2024-04-09"))
        (is (not (str/includes? payload "2024-01-01")))))
    (testing "a missing page is seeded from init-page"
      (is (= :itera-pixi-timeseries
             (first (protocol/to-formatted-paged-hiccup v nil window nil)))))))

(deftest select-rows-resize-test
  (let [rows (ohlc-rows 240)]
    (testing "a narrower window re-derives the slice, ignoring stale :rows-visible"
      (let [page  {:display :nice :step 8 :end 240 :rows-all 240 :rows-visible 75}
            slice (pixi/select-rows {:window {:width 300 :height 400}
                                     :window-selector page} rows)]
        (is (= 37 (count slice)))
        (is (= "2024-07-22" (:date (first slice))))
        (is (= "2024-08-27" (:date (last slice))))))
    (testing "a wider window derives more rows than the stored value"
      (let [page  {:display :nice :step 8 :end 240 :rows-all 240 :rows-visible 10}
            slice (pixi/select-rows {:window {:width 800 :height 400}
                                     :window-selector page} rows)]
        (is (= 100 (count slice)))))
    (testing "a missing/invalid width falls back to the stored :rows-visible"
      (let [page  {:display :nice :step 8 :end 240 :rows-all 240 :rows-visible 50}]
        (is (= 50 (count (pixi/select-rows {:window {:height 400}
                                            :window-selector page} rows))))))))

;; ---------------------------------------------------------------------------
;; invalid spec
;; ---------------------------------------------------------------------------

(deftest invalid-spec-test
  (let [v (pixi/pixi-timeseries (ohlc-rows 3))]
    (testing "an empty map spec renders the invalid block"
      (is (= fmt/invalid-spec-block
             (protocol/to-formatted-hiccup v {} nil))))
    (testing "a scalar spec renders the invalid block"
      (is (= fmt/invalid-spec-block
             (protocol/to-formatted-hiccup v 42 nil))))
    (testing "the smart default renders the viewer"
      (is (= :itera-pixi-timeseries
             (first (protocol/to-formatted-hiccup v nil window)))))))