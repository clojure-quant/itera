(ns itera.util.clj-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [itera.util.clj :as util]))

(def ^:private workbook-nss
  '[itera.util.clj-test.workbook-requires
    itera.util.clj-test.workbook-bare])

(defn- cleanup-workbooks! []
  (doseq [n workbook-nss]
    (when (find-ns n)
      (remove-ns n))))

(use-fixtures :each
  (fn [f]
    (let [original-ns *ns*]
      (cleanup-workbooks!)
      (try
        (binding [*ns* *ns*]
          (f))
        (finally
          (in-ns (ns-name original-ns))
          (cleanup-workbooks!))))))

(deftest create-workbook-test
  (testing "creates a namespace and applies requires"
    (let [wb 'itera.util.clj-test.workbook-requires
          result (util/create-workbook wb '[[clojure.string :as string]])]
      (is (nil? (:ex result)))
      (is (nil? (:value result)))
      (is (= "" (:out result)))
      (is (= wb (ns-name (find-ns wb))))
      (is (= "ABC" (:value (util/compile-formula (str wb) "(string/upper-case \"abc\")"))))))
  (testing "creates a bare namespace when requires is empty"
    (let [wb 'itera.util.clj-test.workbook-bare
          result (util/create-workbook wb nil)]
      (is (nil? (:ex result)))
      (is (= wb (ns-name (find-ns wb)))))))

(deftest create-workbook-ns-code-test
  (testing "formats each required libspec"
    (is (= "(ns itera.util.clj-test.workbook (:require [clojure.string :as string]\n  [clojure.edn :as edn]))"
           (util/create-workbook-ns-code
            "itera.util.clj-test.workbook"
            '[[clojure.string :as string] [clojure.edn :as edn]]))))
  (testing "emits a bare ns form without requires"
    (is (= "(ns itera.util.clj-test.workbook)"
           (util/create-workbook-ns-code
            "itera.util.clj-test.workbook" nil)))))

(deftest compile-formula-success-test
  (let [wb 'itera.util.clj-test.workbook-bare
        _ (util/create-workbook wb nil)
        code "(fn [ctx assets] (println assets))"
        result (util/compile-formula (str wb) code)]
    (is (nil? (:ex result)))
    (is (= code (:code result)))
    (is (= (str wb) (:ns result)))
    (is (fn? (:value result)))
    (is (= "" (:out result)))))

(deftest compile-formula-captures-output-test
  (let [wb 'itera.util.clj-test.workbook-bare
        _ (util/create-workbook wb nil)
        result (util/compile-formula (str wb) "(println \"hello\")")]
    (is (nil? (:ex result)))
    (is (nil? (:value result)))
    (is (= "hello\n" (:out result)))))

(deftest compile-formula-compile-error-test
  (let [wb 'itera.util.clj-test.workbook-bare
        _ (util/create-workbook wb nil)
        result (util/compile-formula (str wb) "(fn [ctx assets] (println assets2))")]
    (is (contains? result :ex))
    (is (instance? clojure.lang.ExceptionInfo (:ex result)))
    (is (re-find #"assets2" (ex-message (:ex result))))
    (is (zero? (alength (.getStackTrace ^Throwable (:ex result)))))
    (is (not (contains? result :value)))))

(deftest fn-arglist-test
  (testing "returns argument vectors for anonymous fn forms"
    (is (= '[ctx assets]
           (util/fn-arglist "(fn [ctx assets] (println assets))")))
    (is (= '[_ assets]
           (util/fn-arglist "(fn [_ assets] (println assets))"))))
  (testing "handles reader macros in the fn body (deref)"
    (is (= '[_ assets]
           (util/fn-arglist "(fn [_ assets] @assets)"))))
  (testing "returns nil for non-fn forms"
    (is (nil? (util/fn-arglist "(println :not-a-fn)")))
    (is (nil? (util/fn-arglist "[ctx assets]")))))