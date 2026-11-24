(ns itera.plot.pixi-timeseries.plot-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [itera.plot.pixi-timeseries.pane-layout :as pane-layout]
   [itera.plot.pixi-timeseries.plot :as pixi]
   [itera.plot.pixi-timeseries.plot-format-ui :as fmt]
   [itera.plot.pixi-timeseries.plot-type :as plot-type]
   [itera.util.color :as colors]
   [itera.value.protocol :as protocol]
   [tech.v3.dataset :as ds]))

(def ^:private window
  {:width 600 :height 400})

(def ^:private default-heights
  {:main-pane-min-height 200 :aux-pane-height 100})

(defn- ohlc-rows
  [n]
  (mapv (fn [i]
          {:date (str (.plusDays (java.time.LocalDate/of 2024 1 1) i))
           :open (+ 100.0 i)
           :high (+ 102.0 i)
           :low  (+ 99.0 i)
           :close (+ 101.0 i)})
        (range n)))

(defn- bear-rows
  "OHLC rows with a boolean :bear-market column true for a run of rows."
  [n]
  (mapv (fn [i row] (assoc row :bear-market (< 1 i 4)))
        (range n)
        (ohlc-rows n)))

(defn- signal-rows
  "OHLC rows with keyword/false/nil :signal values for the first few rows."
  [n]
  (mapv (fn [i row]
          (assoc row :signal (when (< i 5)
                               (nth [:long :short :close :entry false] i))))
        (range n)
        (ohlc-rows n)))

;; ---------------------------------------------------------------------------
;; default spec
;; ---------------------------------------------------------------------------

(deftest default-spec-ohlc-test
  (is (= (assoc default-heights
                :panes [{:yAxisScale :linear
                         :plots [{:type :bar
                                  :col [:open :high :low :close]}]}])
         (fmt/default-spec* (ohlc-rows 3)))))

(deftest default-spec-line-test
  (testing ":close is preferred for non-OHLC data"
    (is (= (assoc default-heights
                  :panes [{:yAxisScale :linear
                           :plots [{:type :line :col :close}]}])
           (fmt/default-spec* [{:date "2024-01-01" :close 1.5}]))))
  (testing "the first numeric non-time column is used"
    (is (= (assoc default-heights
                  :panes [{:yAxisScale :linear
                           :plots [{:type :line :col :volume}]}])
           (fmt/default-spec* [{:date "2024-01-01" :asset "a" :volume 3}]))))
  (testing "time and non-numeric columns are skipped"
    (is (= (assoc default-heights :panes [])
           (fmt/default-spec* [{:epoch 1 :date "2024-01-01"
                                :asset "a" :entry :flat}])))))

(deftest default-spec-empty-test
  (is (= (assoc default-heights :panes []) (fmt/default-spec* [])))
  (is (= (assoc default-heights :panes []) (fmt/default-spec* nil))))

(deftest default-spec-protocol-test
  (let [rows (ohlc-rows 3)]
    (is (= (fmt/default-spec* rows)
           (protocol/default-spec (pixi/pixi-timeseries rows))))))

;; ---------------------------------------------------------------------------
;; plot types
;; ---------------------------------------------------------------------------

(deftest point-drop-type-test
  (is (some #{:point-drop} plot-type/types)))

(deftest background-color-type-test
  (is (some #{:background-color} plot-type/types)))

(deftest signal-type-test
  (is (some #{:signal} plot-type/types)))

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
      (is (= (assoc default-heights
                    :panes [{:yAxisScale :linear
                             :plots [{:type :bar
                                      :col [:open :high :low :close]}]}])
             (protocol/default-spec v))))
    (testing "the preformatted path works from seq data"
      (is (= :itera-pixi-timeseries
             (first (protocol/to-formatted-hiccup v nil window)))))
    (testing "paging works from seq data"
      (is (= {:display :nice
              :step 8
              :aux-pane 0
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
      (is (= (assoc default-heights
                    :panes [{:yAxisScale :linear
                             :plots [{:type :bar
                                      :col [:open :high :low :close]}]}])
             (protocol/default-spec v)))
      (let [hiccup (protocol/to-formatted-paged-hiccup v nil window nil)]
        (is (= :itera-pixi-timeseries (first hiccup)))
        (is (str/includes? (:options (second hiccup)) "2024-01-12"))))))

;; ---------------------------------------------------------------------------
;; normalize spec
;; ---------------------------------------------------------------------------

(deftest normalize-spec-test
  (testing "canonical spec maps are preserved, including :yAxisScale :log"
    (let [spec {:main-pane-min-height 150
                :aux-pane-height 80
                :panes [{:yAxisScale :log
                         :plots [{:type :line :col :close}]}
                        {:yAxisScale :linear
                         :plots [{:type :range :col [:low :high] :color "red-3"}]}]}]
      (is (= spec (fmt/normalize-spec spec)))))
  (testing "a legacy pane vector is converted to the map shape with defaults"
    (is (= (assoc default-heights
                  :panes [{:yAxisScale :linear :plots [{:type :line :col :close}]}])
           (fmt/normalize-spec [[{:type :line :col :close}]]))))
  (testing "unknown map keys are preserved and heights default when invalid"
    (is (= {:panes [{:yAxisScale :linear :plots [] :extra 1}]
            :extra 1
            :main-pane-min-height 200
            :aux-pane-height 100}
           (fmt/normalize-spec {:extra 1
                                :main-pane-min-height 0
                                :aux-pane-height -5
                                :panes [{:extra 1}]}))))
  (testing "an invalid/missing :yAxisScale becomes :linear"
    (is (= (assoc default-heights
                  :panes [{:yAxisScale :linear :plots []}])
           (fmt/normalize-spec [{:yAxisScale :bogus :plots []}])))
    (is (= (assoc default-heights
                  :panes [{:yAxisScale :linear :plots []}])
           (fmt/normalize-spec [{:plots []}]))))
  (testing "bar plots lose :color"
    (is (= (assoc default-heights
                  :panes [{:yAxisScale :linear
                           :plots [{:type :bar
                                    :col [:open :high :low :close]}]}])
           (fmt/normalize-spec [[{:type :bar :col [:open :high :low :close]
                                  :color "red-3"}]]))))
  (testing "map panes gain :plots []/:yAxisScale and keep unknown keys"
    (is (= (assoc default-heights
                  :panes [{:yAxisScale :linear :plots [] :extra 1}])
           (fmt/normalize-spec [{:extra 1}]))))
  (testing "a non-map plot becomes an empty plot map"
    (is (= (assoc default-heights :panes [{:yAxisScale :linear :plots [{}]}])
           (fmt/normalize-spec [["nope"]]))))
  (testing "scalar panes become empty :linear panes"
    (is (= (assoc default-heights :panes [{:yAxisScale :linear :plots []}])
           (fmt/normalize-spec [42]))))
  (testing "an empty vector is valid"
    (is (= (assoc default-heights :panes []) (fmt/normalize-spec []))))
  (testing ":point-drop keeps its :col, :color and :base-value"
    (let [pane {:yAxisScale :linear
                :plots [{:type :point-drop :col :close
                         :color "red-3" :base-value 95}]}]
      (is (= (assoc default-heights :panes [pane])
             (fmt/normalize-spec {:panes [pane]})))))
  (testing ":background-color keeps its :col and :color"
    (let [pane {:yAxisScale :linear
                :plots [{:type :background-color :col :bear-market
                         :color "red-2"}]}]
      (is (= (assoc default-heights :panes [pane])
             (fmt/normalize-spec [pane])))))
  (testing ":signal keeps :y-align and :size and strips :color"
    (let [plot {:type :signal :col :signal :y-align :close :size 10}
          pane {:yAxisScale :linear :plots [plot]}]
      (is (= (assoc default-heights :panes [pane])
             (fmt/normalize-spec
              {:panes [{:yAxisScale :linear
                        :plots [(assoc plot :color "red-3")]}]})))))
  (testing ":signal keeps an explicit nil :y-align"
    (let [pane {:yAxisScale :linear
                :plots [{:type :signal :col :signal :y-align nil :size 8}]}]
      (is (= (assoc default-heights :panes [pane])
             (fmt/normalize-spec [pane])))))
  (testing "maps without a :panes vector, nil and scalars are invalid"
    (is (nil? (fmt/normalize-spec {})))
    (is (nil? (fmt/normalize-spec {:charts []})))
    (is (nil? (fmt/normalize-spec nil)))
    (is (nil? (fmt/normalize-spec 42)))))

(deftest palette-test
  (testing "the palette is the sorted distinct named colors"
    (is (= (vec (sort (keys colors/colors)))
           (fmt/palette* nil))))
  (testing "used colors outside the named table are added in name order"
    (let [spec [{:yAxisScale :linear
                 :plots [{:type :line :col :close :color "custom-1"}]}]]
      (is (= (vec (sort (conj (set (keys colors/colors)) "custom-1")))
             (fmt/palette* spec))))))

;; ---------------------------------------------------------------------------
;; pane layout
;; ---------------------------------------------------------------------------

(def ^:private three-pane-spec
  {:main-pane-min-height 220
   :aux-pane-height 120
   :panes [{:yAxisScale :linear
            :plots [{:type :bar :col [:open :high :low :close]}]}
           {:yAxisScale :linear :plots [{:type :line :col :sine :color "blue-5"}]}
           {:yAxisScale :linear :plots [{:type :line :col :cosine :color "green-5"}]}]})

(deftest normalize-params-test
  (is (= default-heights (pane-layout/normalize-params {})))
  (is (= default-heights
         (pane-layout/normalize-params {:main-pane-min-height 0
                                        :aux-pane-height "x"})))
  (is (= {:main-pane-min-height 300 :aux-pane-height 50}
         (pane-layout/normalize-params {:main-pane-min-height 300
                                        :aux-pane-height 50}))))

(deftest pane-layout-single-test
  (testing "one pane fills the container height"
    (is (= {:main-pane-min-height 200 :aux-height 100
            :main-height 400 :aux-count 0 :visible-aux-count 0
            :start 0 :max-start 0 :indices [0] :heights [400]
            :overflow? false :total-height 400}
           (pane-layout/layout {:panes [{}]} {:height 400} 0)))))

(deftest pane-layout-fitting-test
  (testing "two 120px aux panes and a 280px main pane at h=520"
    (is (= {:main-pane-min-height 220 :aux-height 120
            :main-height 280 :aux-count 2 :visible-aux-count 2
            :start 0 :max-start 0 :indices [0 1 2] :heights [280 120 120]
            :overflow? false :total-height 520}
           (pane-layout/layout three-pane-spec {:height 520} 0))))
  (testing "at h=400 only one aux pane fits; main = h - k*a"
    (is (= {:main-pane-min-height 220 :aux-height 120
            :main-height 280 :aux-count 2 :visible-aux-count 1
            :start 0 :max-start 1 :indices [0 1] :heights [280 120]
            :overflow? true :total-height 400}
           (pane-layout/layout three-pane-spec {:height 400} 0))))
  (testing "the aux start clamps to the last full window"
    (is (= [0 2] (:indices (pane-layout/layout three-pane-spec {:height 400} 1))))
    (is (= [0 2] (:indices (pane-layout/layout three-pane-spec {:height 400} 9))))))

(deftest pane-layout-floor-test
  (testing "k uses quotient/floor: h=399 still fits one aux pane"
    (is (= [0 1] (:indices (pane-layout/layout three-pane-spec {:height 399} 0)))))
  (testing "h < main-pane-min-height shows only the main pane at full height"
    (let [plan (pane-layout/layout three-pane-spec {:height 150} 0)]
      (is (= 0 (:visible-aux-count plan)))
      (is (= [0] (:indices plan)))
      (is (= [150] (:heights plan)))
      (is (:overflow? plan)))))

(deftest pane-layout-invalid-height-test
  (testing "an invalid window height lists all panes with nil heights"
    (let [plan (pane-layout/layout three-pane-spec {:width 600} 0)]
      (is (= [0 1 2] (:indices plan)))
      (is (nil? (:heights plan)))
      (is (nil? (:main-height plan)))
      (is (false? (:overflow? plan))))))

(deftest pane-layout-empty-test
  (is (= [] (:indices (pane-layout/layout {:panes []} {:height 400} 0))))
  (is (= [] (:heights (pane-layout/layout {:panes []} {:height 400} 0)))))

(deftest aux-pane-payload-test
  (let [v       (pixi/pixi-timeseries (ohlc-rows 3))
        payload (fn [aux-start]
                  (let [page   {:display :nice :step 8 :end 3 :rows-all 3
                                :rows-visible 3 :aux-pane aux-start}
                        hiccup (protocol/to-formatted-paged-hiccup
                                v three-pane-spec window page)]
                    (str (:options (second hiccup)))))
        yscales (fn [s] (count (re-seq #"yAxisScale" s)))]
    (testing "aux-start 0 sends the bar main pane plus the sine aux pane"
      (let [s (payload 0)]
        (is (= 2 (yscales s)))
        (is (str/includes? s "\"heights\":[280,120]"))
        (is (str/includes? s "\"col\":\"sine\""))
        (is (not (str/includes? s "\"col\":\"cosine\"")))))
    (testing "aux-start 1 sends the cosine aux pane and no sine"
      (let [s (payload 1)]
        (is (= 2 (yscales s)))
        (is (str/includes? s "\"heights\":[280,120]"))
        (is (str/includes? s "\"col\":\"cosine\""))
        (is (not (str/includes? s "\"col\":\"sine\"")))))))

(deftest log-pane-payload-test
  (let [v       (pixi/pixi-timeseries (ohlc-rows 3))
        hiccup  (protocol/to-formatted-hiccup
                 v [{:yAxisScale :log
                     :plots [{:type :line :col :close}]}]
                 window)
        payload (:options (second hiccup))]
    (is (= :itera-pixi-timeseries (first hiccup)))
    (is (str/includes? payload "yAxisScale"))
    (is (str/includes? payload "log"))))

(deftest point-drop-payload-test
  (let [v      (pixi/pixi-timeseries (ohlc-rows 3))
        hiccup (protocol/to-formatted-hiccup
                v [{:yAxisScale :linear
                    :plots [{:type :point-drop :col :close
                             :color "red-3" :base-value 87.5}]}]
                window)
        payload (:options (second hiccup))]
    (is (= :itera-pixi-timeseries (first hiccup)))
    (is (str/includes? payload "point-drop"))
    (is (str/includes? payload "base-value"))
    (is (str/includes? payload "87.5"))))

(deftest background-color-payload-test
  (let [v      (pixi/pixi-timeseries (bear-rows 6))
        hiccup (protocol/to-formatted-hiccup
                v [{:yAxisScale :linear
                    :plots [{:type :background-color :col :bear-market
                             :color "red-2"}]}]
                window)
        payload (:options (second hiccup))]
    (testing "a boolean column keeps the plot with its resolved color"
      (is (= :itera-pixi-timeseries (first hiccup)))
      (is (str/includes? payload "background-color"))
      (is (str/includes? payload "#fecaca")))))

(deftest background-color-ignored-test
  (let [v      (pixi/pixi-timeseries (bear-rows 6))
        hiccup (protocol/to-formatted-hiccup
                v [{:yAxisScale :linear
                    :plots [{:type :background-color :col :close
                             :color "red-2"}]}]
                window)
        payload (:options (second hiccup))]
    (testing "a non-boolean column drops the plot server-side"
      (is (= :itera-pixi-timeseries (first hiccup)))
      (is (not (str/includes? payload "background-color"))))))

(deftest signal-payload-test
  (let [v      (pixi/pixi-timeseries (signal-rows 6))
        hiccup (protocol/to-formatted-hiccup
                v [{:yAxisScale :linear
                    :plots [{:type :signal :col :signal
                             :y-align :close :size 10}]}]
                window)
        payload (:options (second hiccup))]
    (is (= :itera-pixi-timeseries (first hiccup)))
    (testing "the signal plot passes through with y-align and size"
      (is (str/includes? payload "signal"))
      (is (str/includes? payload "y-align"))
      (is (str/includes? payload "\"y-align\":\"close\""))
      (is (str/includes? payload "size"))
      (is (str/includes? payload "10")))
    (testing "keyword signal values are serialized as plain strings"
      (is (str/includes? payload "\"signal\":\"long\""))
      (is (str/includes? payload "\"signal\":\"short\""))
      (is (str/includes? payload "\"signal\":\"close\""))
      (is (str/includes? payload "\"signal\":\"entry\""))
      (is (str/includes? payload "\"signal\":false")))
    (testing "the y-align column is present in the payload rows"
      (is (str/includes? payload "\"close\":")))))

;; ---------------------------------------------------------------------------
;; page protocol
;; ---------------------------------------------------------------------------

(deftest init-page-test
  (let [v    (pixi/pixi-timeseries (ohlc-rows 240))
        page (protocol/init-page v nil window)]
    (is (= {:display :nice
            :step 8
            :aux-pane 0
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