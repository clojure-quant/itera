(ns itera.plot.fipp-test
  (:require
   [clojure.edn :as edn]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [itera.plot.fipp :as fipp]
   [itera.plot.fipp-format-ui :as fipp-fmt]
   [itera.value.protocol :as protocol]))

(defn- text
  "Pprinted text from a to-formatted-hiccup result with SPEC and WINDOW."
  ([value spec] (text value spec nil))
  ([value spec window]
   (nth (protocol/to-formatted-hiccup value spec window) 2)))

(deftest default-spec-test
  (is (= {:width        70
          :print-length nil
          :print-level  nil
          :print-meta   false}
         (protocol/default-spec (fipp/fipp {:a 1})))))

(deftest to-hiccup-test
  (let [[tag attrs s] (protocol/to-hiccup (fipp/fipp {:a 1 :b [2 3]}))]
    (is (= :pre tag))
    (is (map? attrs))
    (is (str/includes? s ":a 1"))
    (is (str/includes? s ":b [2 3]"))))

(deftest to-hiccup-preserves-style-test
  (let [[_ attrs _] (protocol/to-hiccup (fipp/fipp {:a 1}))]
    (is (= "#d0d6fd" (:background (:style attrs))))))

(deftest width-test
  (let [value  (fipp/fipp {:alpha 1 :beta 2 :gamma [1 2 3 4 5 6 7 8]})
        wide   (text value {:width 70})
        narrow (text value {:width 10})]
    (is (< (count (str/split-lines wide)) (count (str/split-lines narrow))))
    (is (< 2 (count (str/split-lines narrow))))))

(deftest print-length-test
  (let [s (text (fipp/fipp (vec (range 10))) {:print-length 3})]
    (is (str/includes? s "..."))
    (is (str/includes? s "0"))
    (is (str/includes? s "2"))
    (is (not (str/includes? s "9")))))

(deftest print-level-test
  (let [value (fipp/fipp {:a {:b {:c 1}}})
        deep  (text value {:print-level 1})
        full  (text value {})]
    (is (str/includes? deep "#"))
    (is (not (str/includes? full "#")))))

(deftest print-meta-test
  (let [value   (fipp/fipp (with-meta [1 2] {:tag :foo}))
        with    (text value {:print-meta true})
        without (text value {:print-meta false})]
    (is (str/includes? with "^"))
    (is (not (str/includes? without "^")))))

(deftest nil-spec-uses-defaults-test
  (is (= (text (fipp/fipp {:a 1}) nil)
         (text (fipp/fipp {:a 1}) fipp-fmt/default-options))))

(deftest window-height-test
  (let [[_ attrs _] (protocol/to-formatted-hiccup
                     (fipp/fipp {:a 1}) nil {:width 600 :height 400})]
    (is (= "400px" (get-in attrs [:style :max-height])))
    (is (= "100%" (get-in attrs [:style :width])))
    (is (= "0" (get-in attrs [:style :margin])))))

(deftest invalid-spec-test
  (let [[tag _ s] (protocol/to-formatted-hiccup
                   (fipp/fipp {:a 1}) [:not :a :map] nil)]
    (is (= :div tag))
    (is (str/includes? s "Invalid fipp spec"))))

(deftest normalize-options-test
  (testing "nil means defaults"
    (is (= fipp-fmt/default-options
           (fipp-fmt/normalize-options nil))))
  (testing "non-map is invalid"
    (is (nil? (fipp-fmt/normalize-options [:not :a :map])))
    (is (nil? (fipp-fmt/normalize-options "spec"))))
  (testing "supported values are kept and unknown keys are dropped"
    (is (= {:width 30 :print-length 5 :print-meta true}
           (fipp-fmt/normalize-options
            {:width 30 :print-length 5 :print-level nil
             :print-meta true :symbols {}}))))
  (testing "zero counts are valid, zero width is not"
    (is (= {:print-length 0 :print-level 0}
           (fipp-fmt/normalize-options {:width 0
                                        :print-length 0
                                        :print-level 0}))))
  (testing "values of the wrong type are dropped"
    (is (= {}
           (fipp-fmt/normalize-options
            {:width -1 :print-length -2 :print-level "x" :print-meta nil})))))

(deftest editor-read-spec-test
  (is (= fipp-fmt/default-options
         (#'fipp-fmt/read-spec nil (atom ""))))
  (is (= {:width 30}
         (#'fipp-fmt/read-spec nil (atom "{:width 30}"))))
  (is (= {:width 30}
         (#'fipp-fmt/read-spec {:width 30} (atom "not edn")))))

(deftest editor-width-test
  (let [code-a (atom (pr-str {:width 40 :custom true}))]
    (#'fipp-fmt/set-width! {:width 40 :custom true} code-a "60")
    (is (= {:width 60 :custom true} (edn/read-string @code-a)))
    (#'fipp-fmt/set-width! {:width 60 :custom true} code-a "")
    (is (= {:custom true} (edn/read-string @code-a)))))

(deftest editor-invalid-number-leaves-spec-test
  (let [code-a (atom (pr-str {:width 40 :print-length 5}))]
    (#'fipp-fmt/set-width! {:width 40 :print-length 5} code-a "nope")
    (is (= {:width 40 :print-length 5} (edn/read-string @code-a)))
    (#'fipp-fmt/set-width! {:width 40 :print-length 5} code-a "0")
    (is (= {:width 40 :print-length 5} (edn/read-string @code-a)))
    (#'fipp-fmt/set-count! {:width 40 :print-length 5} code-a :print-length "-1")
    (is (= {:width 40 :print-length 5} (edn/read-string @code-a)))))

(deftest editor-count-test
  (let [code-a (atom (pr-str {:print-length 5 :print-level 2}))]
    (#'fipp-fmt/set-count! {:print-length 5 :print-level 2}
                           code-a :print-length "0")
    (is (= {:print-length 0 :print-level 2} (edn/read-string @code-a)))
    (#'fipp-fmt/set-count! {:print-length 0 :print-level 2}
                           code-a :print-level "")
    (is (= {:print-length 0} (edn/read-string @code-a)))))

(deftest editor-print-meta-test
  (let [code-a (atom (pr-str {:width 40}))]
    (#'fipp-fmt/set-print-meta! {:width 40} code-a true)
    (is (= {:width 40 :print-meta true} (edn/read-string @code-a)))
    (#'fipp-fmt/set-print-meta! {:width 40 :print-meta true} code-a false)
    (is (= {:width 40 :print-meta false} (edn/read-string @code-a)))))