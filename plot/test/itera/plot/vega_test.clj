(ns itera.plot.vega-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [itera.plot.vega :as vega]
   [itera.value.protocol :as protocol]))

(def rows [{:x 1 :y 2} {:x 3 :y 4}])

(def datasets {:table rows})

(deftest record-shape-test
  (let [vl (vega/vegalite rows)
        v  (vega/vega rows)]
    (is (= rows (:data vl)))
    (is (= :vegalite (:mode vl)))
    (is (= rows (:data v)))
    (is (= :vega (:mode v)))))

(deftest default-spec-test
  (is (nil? (protocol/default-spec (vega/vegalite rows))))
  (is (nil? (protocol/default-spec (vega/vega nil)))))

(deftest embed-options-vegalite-rows-test
  (let [{:keys [spec embedOpts]} (#'vega/embed-options rows :vegalite nil nil)]
    (is (= {:values rows} (:data spec)))
    (is (= {} embedOpts))))

(deftest embed-options-datasets-test
  (testing "vegalite map data goes into the spec's :datasets"
    (let [{:keys [spec embedOpts]} (#'vega/embed-options datasets :vegalite nil nil)]
      (is (= datasets (:datasets spec)))
      (is (= {} embedOpts))))
  (testing "vega map data fills matching :data entries with :values"
    (let [chart-spec {:data [{:name "table" :url "/r/data/foo"}]}
          {:keys [spec embedOpts]} (#'vega/embed-options datasets :vega chart-spec nil)]
      (is (= [{:name "table" :values rows}] (:data spec)))
      (is (= {:mode "vega"} embedOpts)))))

(deftest embed-options-vega-mode-test
  (is (= {:mode "vega"}
         (:embedOpts (#'vega/embed-options nil :vega nil nil))))
  (is (= {} (:embedOpts (#'vega/embed-options nil :vegalite nil nil)))))

(deftest embed-options-window-test
  (let [{:keys [spec]} (#'vega/embed-options
                        nil :vegalite {:width 100 :height 100 :mark "line"}
                        {:width 600 :height 400})]
    (is (= 600 (:width spec)))
    (is (= 400 (:height spec)))
    (is (= "line" (:mark spec)))))

(deftest embed-options-nil-spec-test
  (is (= {} (:spec (#'vega/embed-options nil :vegalite nil nil))))
  (is (= {} (:spec (#'vega/embed-options rows :vega {} nil)))))

(deftest embed-options-window-autosize-test
  (let [window {:width 600 :height 400}
        spec-with (fn [autosize]
                    (:spec (#'vega/embed-options
                            nil :vega (cond-> {} (some? autosize) (assoc :autosize autosize))
                            window)))]
    (testing "default autosize becomes fit + contains padding"
      (is (= {:type "fit" :contains "padding"} (:autosize (spec-with nil)))))
    (testing "explicit pad becomes fit + contains padding"
      (is (= {:type "fit" :contains "padding"} (:autosize (spec-with "pad")))))
    (testing "fit becomes fit + contains padding"
      (is (= {:type "fit" :contains "padding"} (:autosize (spec-with "fit")))))
    (testing "fit-x keeps its axis and gains contains padding"
      (is (= {:type "fit-x" :contains "padding"} (:autosize (spec-with "fit-x")))))
    (testing "none is left alone"
      (is (= "none" (:autosize (spec-with "none")))))
    (testing "a map type keeps other keys"
      (is (= {:type "fit" :contains "padding" :resize true}
             (:autosize (spec-with {:type "fit" :resize true})))))
    (testing "a signal-valued autosize is left alone"
      (is (= {:signal "as"} (:autosize (spec-with {:signal "as"})))))))

(deftest embed-options-window-autosize-scope-test
  (testing "no window leaves the spec untouched"
    (is (= {} (:spec (#'vega/embed-options nil :vega {} nil)))))
  (testing "vega-lite is not given an autosize"
    (is (not (contains? (:spec (#'vega/embed-options
                                nil :vegalite {:width 100 :height 100}
                                {:width 600 :height 400}))
                        :autosize)))))

(deftest embed-options-vega-rows-untouched-test
  (let [{:keys [spec embedOpts]} (#'vega/embed-options rows :vega nil nil)]
    (is (not (contains? spec :data)))
    (is (= {:mode "vega"} embedOpts))))

(deftest to-formatted-hiccup-test
  (is (some? (protocol/to-formatted-hiccup
              (vega/vegalite rows)
              {:mark "point" :encoding {:x {:field "x"}}}
              {:width 600 :height 400})))
  (is (some? (protocol/to-formatted-hiccup
              (vega/vega nil)
              {:$schema "https://vega.github.io/schema/vega/v5.json"
               :width 100 :height 100 :data [] :marks []}
              {:width 600 :height 400}))))

(deftest to-hiccup-test
  (is (some? (protocol/to-hiccup (vega/vegalite rows))))
  (is (some? (protocol/to-hiccup (vega/vega nil)))))