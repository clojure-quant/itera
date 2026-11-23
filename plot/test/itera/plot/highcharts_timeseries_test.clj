(ns itera.plot.highcharts-timeseries-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [itera.plot.highcharts-timeseries :as ts]
   [itera.plot.highcharts-timeseries-format-ui :as fmt]
   [itera.plot.highcharts-timeseries-page-editor :as page]
   [itera.value.protocol :as protocol]
   [tech.v3.dataset :as ds]))

(def ^:private rows
  [{:epoch 1000 :open 10 :high 12 :low 9 :close 11 :volume 100
    :low2 8 :high2 13 :pivot [1 2] :entry :long}
   {:epoch 2000 :open 11 :high 13 :low 10 :close 12 :volume 150
    :low2 9 :high2 14 :pivot nil :entry :short}
   {:epoch 3000 :open 12 :high 14 :low 11 :close 13 :volume 200
    :low2 10 :high2 15 :pivot [3] :entry :flat}])

(defn- options
  [plots]
  (ts/build-options {:plots plots} rows))

(defn- series-options
  [series]
  (get-in (options [{:height 200 :series [series]}]) [:series 0]))

(defn- code
  [code-a]
  (edn/read-string @code-a))

;; ---------------------------------------------------------------------------
;; default spec
;; ---------------------------------------------------------------------------

(deftest default-spec-ohlc-test
  (let [spec (ts/default-spec* rows)]
    (is (= 2 (count (:plots spec))))
    (is (= 400 (get-in spec [:plots 0 :height])))
    (is (= :ohlc (get-in spec [:plots 0 :series 0 :type])))
    (is (= [:open :high :low :close]
           (get-in spec [:plots 0 :series 0 :columns])))
    (is (= :candle (get-in spec [:plots 0 :series 0 :mode])))
    (is (= 120 (get-in spec [:plots 1 :height])))
    (is (= :column (get-in spec [:plots 1 :series 0 :type])))
    (is (= [:volume] (get-in spec [:plots 1 :series 0 :columns])))))

(deftest default-spec-line-test
  (testing ":close is preferred for non-OHLC data"
    (is (= {:plots [{:height 400
                     :series [{:type :line :columns [:close]}]}]}
           (ts/default-spec* [{:epoch 1 :name "a" :close 1.5}]))))
  (testing "the first numeric non-time column is used"
    (is (= [{:type :line :columns [:volume]}]
           (get-in (ts/default-spec* [{:epoch 1 :asset "a" :volume 3}])
                   [:plots 0 :series]))))
  (testing ":close wins over earlier numeric columns"
    (is (= [{:type :line :columns [:close]}]
           (get-in (ts/default-spec* [{:epoch 1 :open 4 :close 5}])
                   [:plots 0 :series])))))

(deftest default-spec-empty-test
  (is (= {:plots []}
         (ts/default-spec* [{:epoch 1 :asset "a" :entry :flat}])))
  (is (= {:plots []} (ts/default-spec* []))))

(deftest default-spec-protocol-test
  (is (= (ts/default-spec* rows)
         (protocol/default-spec (ts/highcharts-timeseries rows)))))

(deftest default-spec-normalizes-test
  (is (= (ts/default-spec* rows)
         (fmt/normalize-spec (ts/default-spec* rows))))
  (is (= {:plots []}
         (fmt/normalize-spec (ts/default-spec* [{:epoch 1 :asset "a"}])))))

;; ---------------------------------------------------------------------------
;; normalize-spec
;; ---------------------------------------------------------------------------

(deftest normalize-spec-invalid-test
  (is (nil? (fmt/normalize-spec nil)))
  (is (nil? (fmt/normalize-spec [:not :a :map])))
  (is (nil? (fmt/normalize-spec {})))
  (is (nil? (fmt/normalize-spec {:plots :whatever}))))

(deftest normalize-spec-shape-test
  (is (= {:plots []} (fmt/normalize-spec {:plots []})))
  (is (= {:plots [{:series []}]} (fmt/normalize-spec {:plots [{}]})))
  (is (= 1 (:custom (fmt/normalize-spec {:custom 1 :plots []}))))
  (is (= 2 (get-in (fmt/normalize-spec
                    {:plots [{:series [{:column :close :foo 2}]}]})
                   [:plots 0 :series 0 :foo]))))

(deftest normalize-spec-columns-test
  (testing "singular :column becomes :columns"
    (is (= [{:type :line :columns [:close]}]
           (get-in (fmt/normalize-spec
                    {:plots [{:series [{:column :close}]}]})
                   [:plots 0 :series]))))
  (testing "a bare keyword is a column shorthand"
    (is (= [{:type :line :columns [:close]}]
           (get-in (fmt/normalize-spec {:plots [{:series [:close]}]})
                   [:plots 0 :series]))))
  (testing "fixed-width types get their column defaults"
    (is (= [:open :high :low :close]
           (get-in (fmt/normalize-spec
                    {:plots [{:series [{:type :ohlc}]}]})
                   [:plots 0 :series 0 :columns])))
    (is (= [:high :low :close]
           (get-in (fmt/normalize-spec
                    {:plots [{:series [{:type "hlc"}]}]})
                   [:plots 0 :series 0 :columns]))))
  (testing "range types default to no columns"
    (is (= []
           (get-in (fmt/normalize-spec
                    {:plots [{:series [{:type :range}]}]})
                   [:plots 0 :series 0 :columns])))))

;; ---------------------------------------------------------------------------
;; build-options
;; ---------------------------------------------------------------------------

(deftest single-column-types-test
  (doseq [[type expected]
          [[:line "line"]
           [:step "line"]
           [:column "column"]
           [:spline "spline"]
           [:area "area"]
           [:areaspline "areaspline"]]]
    (testing (str type)
      (let [series (series-options {:type type :columns [:close]})]
        (is (= expected (:type series)))
        (is (= [[1000 11] [2000 12] [3000 13]] (:data series))))))
  (is (true? (:step (series-options {:type :step :columns [:close]}))))
  (is (= 0.5 (:fillOpacity
              (series-options {:type :area :columns [:close] :opacity 0.5})))))

(deftest range-types-test
  (testing ":range uses two columns as [epoch low high]"
    (let [series (series-options {:type :range
                                  :columns [:low2 :high2]})]
      (is (= "arearange" (:type series)))
      (is (= [[1000 8 13] [2000 9 14] [3000 10 15]] (:data series)))))
  (testing "a single pair-valued column is preserved"
    (let [series (series-options {:type :range :columns [:pivot]})]
      (is (= [[1000 1 2] [2000 nil nil] [3000 3 nil]] (:data series)))))
  (testing ":columnrange uses [epoch low high] and an explicit opacity"
    (let [series (series-options {:type :columnrange
                                  :columns [:low2 :high2]
                                  :opacity 0.25})]
      (is (= "columnrange" (:type series)))
      (is (= 0.25 (:opacity series)))
      (is (= [1000 8 13] (first (:data series)))))))

(deftest hlc-test
  (let [series (series-options {:type :hlc
                                :columns [:high :low :close]})]
    (is (= "hlc" (:type series)))
    (is (= [[1000 12 9 11] [2000 13 10 12] [3000 14 11 13]]
           (:data series)))
    (is (= "hlc" (:type series)))))

(deftest ohlc-test
  (testing "OHLC columns map onto x/open/high/low/close"
    (let [series (series-options {:type :ohlc
                                  :columns [:open :high :low :close]
                                  :mode :candle})]
      (is (= "candlestick" (:type series)))
      (is (= {:x 1000 :open 10 :high 12 :low 9 :close 11}
             (first (:data series))))))
  (testing "modes map to Highcharts series types"
    (is (= "hollowcandlestick"
           (:type (series-options {:type :ohlc
                                   :columns [:open :high :low :close]
                                   :mode :candle-hollow}))))
    (is (= "ohlc"
           (:type (series-options {:type :ohlc
                                   :columns [:open :high :low :close]
                                   :mode :ohlc})))))
  (testing ":barcolor colors matching rows via itera.util.color"
    (let [series (series-options {:type :ohlc
                                  :columns [:open :high :low :close]
                                  :barcolor {:column :entry
                                             :color {:long "green-5"}}})]
      (is (= "#22c55e" (:color (first (:data series)))))
      (is (not (contains? (second (:data series)) :color))))))

(deftest point-test
  (testing "scalar :point values"
    (is (= [[1000 11] [2000 12] [3000 13]]
           (:data (series-options {:type :point :columns [:close]})))))
  (testing ":vec? expands sequence values"
    (is (= [[1000 1] [1000 2] [3000 3]]
           (:data (series-options {:type :point
                                   :columns [:pivot]
                                   :vec? true})))))
  (testing "marker styling"
    (let [series (series-options {:type :point
                                  :columns [:close]
                                  :marker "square"
                                  :radius 5})]
      (is (= "scatter" (:type series)))
      (is (= "square" (get-in series [:marker :symbol])))
      (is (= 5 (get-in series [:marker :radius]))))))

(deftest flags-test
  (testing "flag rows are filtered by their v2style key"
    (let [series (series-options {:type :flags :columns [:entry]})]
      (is (= "flags" (:type series)))
      (is (= [1000 2000] (mapv :x (:data series))))
      (is (= ["url(/itera/plot/flags/arrow-up.svg)"
              "url(/itera/plot/flags/arrow-down.svg)"]
             (mapv :shape (:data series))))))
  (testing "flags auto-target the first :ohlc series in their plot"
    (let [opts (options [{:series [{:type :ohlc
                                    :columns [:open :high :low :close]
                                    :name "ohlc"}
                                   {:type :flags :columns [:entry]}]}])]
      (is (= "ohlc" (get-in opts [:series 1 :onSeries])))))
  (testing "an explicit :on-series wins"
    (let [opts (options [{:series [{:type :ohlc
                                    :columns [:open :high :low :close]
                                    :name "ohlc"}
                                   {:type :flags
                                    :columns [:entry]
                                    :on-series "custom"}]}])]
      (is (= "custom" (get-in opts [:series 1 :onSeries]))))))

(deftest multi-plot-axes-test
  (let [opts (options [{:height 300
                        :series [{:type :line :columns [:close]}]}
                       {:height 100
                        :series [{:type :column :columns [:volume]}]}])]
    (testing "chart height is the sum of the plot heights"
      (is (= 400 (get-in opts [:chart :height]))))
    (testing "each plot becomes one Y axis"
      (is (= [{:top 0 :height 300 :resize {:enabled true}}
              {:top 300 :height 100 :resize {:enabled false}}]
             (mapv #(select-keys % [:top :height :resize]) (:yAxis opts)))))
    (testing "series point at their plot's axis"
      (is (= [0 1] (mapv :yAxis (:series opts)))))))

(deftest default-plot-height-test
  (let [opts (options [{:series [{:type :line :columns [:close]}]}])]
    (is (= ts/default-plot-height (get-in opts [:yAxis 0 :height])))))

;; ---------------------------------------------------------------------------
;; hiccup rendering
;; ---------------------------------------------------------------------------

(deftest no-rows-test
  (let [v (ts/highcharts-timeseries [])]
    (is (= "No rows in this chart."
           (nth (protocol/to-hiccup v) 2)))
    (is (= "No rows in this chart."
           (nth (protocol/to-formatted-hiccup v {:plots []} nil) 2)))))

(deftest invalid-spec-test
  (let [[tag _ message] (protocol/to-formatted-hiccup
                         (ts/highcharts-timeseries rows)
                         [:not :a :map]
                         nil)]
    (is (= :div tag))
    (is (str/includes? message "Invalid highcharts-timeseries spec"))))

(deftest valid-spec-hiccup-test
  (let [spec {:plots [{:height 200
                       :series [{:type :ohlc
                                 :columns [:open :high :low :close]}]}]}
        [tag attrs] (protocol/to-formatted-hiccup
                     (ts/highcharts-timeseries rows) spec nil)]
    (is (= :itera-highstock tag))
    (is (string? (:options attrs)))
    (is (str/includes? (:options attrs) "\"candlestick\""))
    (is (str/includes? (:options attrs) "\"height\":200"))
    (is (= "200px" (get-in attrs [:style :height])))))

(deftest to-hiccup-default-test
  (let [[tag attrs] (protocol/to-hiccup (ts/highcharts-timeseries rows))]
    (is (= :itera-highstock tag))
    (is (str/includes? (:options attrs) "\"candlestick\""))
    (is (str/includes? (:options attrs) "\"column\""))))

;; ---------------------------------------------------------------------------
;; seq and dataset data (protocol-only storage) and paging
;; ---------------------------------------------------------------------------

(def ^:private window {:width 600 :height 400})

(defn- ohlc-rows
  [n]
  (mapv (fn [i] {:epoch i :open 1 :high 2 :low 0 :close 1 :volume 3})
        (range n)))

(defn- elements [tag view]
  (filter #(and (vector? %) (= tag (first %)))
          (tree-seq coll? seq view)))

(deftest seq-data-test
  (let [seq-data (map identity rows)
        v        (ts/highcharts-timeseries seq-data)]
    (testing "the seq is stored as-is, not coerced to a vector"
      (is (= seq-data (:data v)))
      (is (instance? clojure.lang.LazySeq (:data v))))
    (testing "the default spec still picks OHLC + volume"
      (is (= 2 (count (:plots (protocol/default-spec v))))))
    (testing "the preformatted path works from seq data"
      (is (= :itera-highstock
             (first (protocol/to-formatted-hiccup v nil window)))))
    (testing "paging works from seq data"
      (is (= 3 (:rows-all (protocol/init-page v nil window)))))))

(deftest dataset-data-test
  (let [d (ds/->dataset {:epoch  (vec (range 100))
                         :open   (vec (repeat 100 1))
                         :high   (vec (repeat 100 2))
                         :low    (vec (repeat 100 0))
                         :close  (vec (repeat 100 1))
                         :volume (vec (repeat 100 3))})
        v (ts/highcharts-timeseries d)]
    (testing "the dataset is stored as-is"
      (is (identical? d (:data v))))
    (testing "column types are inferred from the dataset datatypes"
      (is (= {:epoch :number :open :number :high :number
              :low :number :close :number :volume :number}
             (protocol/column-info (:data v)))))
    (testing "the default spec picks OHLC + volume"
      (is (= 2 (count (:plots (protocol/default-spec v))))))
    (testing "the preformatted path works from dataset data"
      (is (= :itera-highstock
             (first (protocol/to-formatted-hiccup v nil window)))))))

(deftest page-geometry-test
  (let [v (ts/highcharts-timeseries (ohlc-rows 100))]
    (testing "a 600px window at 8px per row shows 75 of 100 rows"
      (is (= {:step 8 :end 100 :rows-all 100 :rows-visible 75}
             (protocol/init-page v nil window))))))

(deftest paged-slicing-test
  (let [v      (ts/highcharts-timeseries (ohlc-rows 100))
        pg     {:step 8 :end 100 :rows-all 100 :rows-visible 75}
        [tag attrs] (protocol/to-formatted-paged-hiccup v nil window pg)]
    (is (= :itera-highstock tag))
    (testing "the page slice is the last 75 rows ending at the exclusive :end"
      (is (str/includes? (:options attrs) "\"x\":25"))
      (is (not (str/includes? (:options attrs) "\"x\":0"))))))

(deftest page-editor-test
  (let [view     (page/page-editor-view {:step 8 :end 100 :rows-visible 75}
                                        {:width 600} 100 nil)
        range-a  (second (first (elements :input view)))
        select-a (second (first (elements :select view)))
        options  (elements :option view)]
    (testing "the range slider is bounded by the derived window and total"
      (is (= "75" (:min range-a)))
      (is (= "100" (:max range-a)))
      (is (= "100" (:value range-a))))
    (testing "the step select offers the current pixels-per-row"
      (is (some #(= "8" (nth % 2)) options)))
    (testing "the summary names the visible window"
      (is (some #{"Showing 26\u2013100 of 100"}
                (filter string? (flatten view)))))
    (testing "with no page cursor every control is disabled and actionless"
      (is (true? (:disabled range-a)))
      (is (nil? (:data-on:change range-a)))
      (is (true? (:disabled select-a)))
      (is (nil? (:data-on:change select-a))))))

;; ---------------------------------------------------------------------------
;; editor helpers
;; ---------------------------------------------------------------------------

(deftest editor-read-spec-test
  (is (= {:plots []} (#'fmt/read-spec {:code-a (atom "") :spec nil})))
  (is (= {:plots []} (#'fmt/read-spec {:code-a (atom "{:plots []}")})))
  (is (= {:plots []} (#'fmt/read-spec {:code-a (atom "not edn") :spec nil})))
  (is (= {:plots [{:series []}]}
         (#'fmt/read-spec {:code-a (atom "not edn")
                           :spec {:plots [{}]}}))))

(deftest editor-write-spec-test
  (let [code-a (atom "{:plots []}")]
    (#'fmt/write-spec! code-a {:plots [{:series []}]})
    (is (= {:plots [{:series []}]} (code code-a)))))

(deftest editor-add-delete-test
  (let [opts {:code-a (atom (pr-str {:plots []}))}]
    (#'fmt/add-plot! opts [:close])
    (is (= 1 (count (:plots (code (:code-a opts))))))
    (is (= :line (get-in (code (:code-a opts)) [:plots 0 :series 0 :type])))
    (is (= [:close] (get-in (code (:code-a opts)) [:plots 0 :series 0 :columns])))
    (is (= fmt/default-plot-height
           (get-in (code (:code-a opts)) [:plots 0 :height])))
    (#'fmt/add-series! opts 0 [:volume])
    (is (= 2 (count (get-in (code (:code-a opts)) [:plots 0 :series]))))
    (#'fmt/delete-series! opts 0 0)
    (is (= 1 (count (get-in (code (:code-a opts)) [:plots 0 :series]))))
    (#'fmt/delete-plot! opts 0)
    (is (= [] (:plots (code (:code-a opts)))))))

(deftest editor-set-type-test
  (let [opts {:code-a
              (atom (pr-str {:plots [{:series [{:type :ohlc
                                                :columns [:open :high :low :close]}]}]}))}]
    (#'fmt/set-type! opts 0 0 :line)
    (is (= [:open]
           (get-in (code (:code-a opts)) [:plots 0 :series 0 :columns])))
    (#'fmt/set-type! opts 0 0 :hlc)
    (is (= [:open :low :close]
           (get-in (code (:code-a opts)) [:plots 0 :series 0 :columns])))
    (#'fmt/set-type! opts 0 0 :ohlc)
    (is (= [:open :low :close :close]
           (get-in (code (:code-a opts)) [:plots 0 :series 0 :columns])))))

(deftest editor-set-column-test
  (let [opts {:code-a
              (atom (pr-str {:plots [{:series [{:type :range
                                                :columns []}]}]}))}]
    (#'fmt/set-column! opts 0 0 1 "high2")
    (is (= [:close :high2]
           (get-in (code (:code-a opts)) [:plots 0 :series 0 :columns])))))

(deftest editor-edn-inputs-test
  (let [spec {:plots [{:series [{:type :ohlc
                                 :columns [:open :high :low :close]
                                 :barcolor {:column :entry}
                                 :v2style {:long "x"}}]}]}
        opts {:code-a (atom (pr-str spec))}]
    (testing "blank removes the key"
      (#'fmt/set-edn-map! opts 0 0 :barcolor "")
      (is (nil? (get-in (code (:code-a opts))
                        [:plots 0 :series 0 :barcolor]))))
    (testing "invalid EDN is a no-op"
      (let [before @(:code-a opts)]
        (#'fmt/set-edn-map! opts 0 0 :v2style "{:long ")
        (is (= before @(:code-a opts)))))
    (testing "a valid map is stored"
      (#'fmt/set-edn-map! opts 0 0 :v2style "{:long \"y\"}")
      (is (= {:long "y"}
             (get-in (code (:code-a opts))
                     [:plots 0 :series 0 :v2style]))))))

(deftest editor-plot-height-test
  (let [opts {:code-a
              (atom (pr-str {:plots [{:height 200
                                      :series [{:type :line
                                                :columns [:close]}]}]}))}]
    (#'fmt/set-plot-height! opts 0 "250")
    (is (= 250 (get-in (code (:code-a opts)) [:plots 0 :height])))
    (let [before @(:code-a opts)]
      (#'fmt/set-plot-height! opts 0 "nope")
      (is (= before @(:code-a opts))))
    (#'fmt/set-plot-height! opts 0 "")
    (is (nil? (get-in (code (:code-a opts)) [:plots 0 :height])))))