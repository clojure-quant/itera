(ns itera.model.core-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [missionary.core :as m]
   [itera.model.core :as rc]))

(def ^:private graph-names
  ["core-test-compile-error" "core-test-success" "core-test-unconnected-input"])

(defn- graph-nss []
  (map #(symbol (str "itera.user." %)) graph-names))

(defn- cleanup-graph-nss! []
  (doseq [n (graph-nss)]
    (when (find-ns n)
      (remove-ns n))))

(use-fixtures :each
  (fn [f]
    (let [original-ns *ns*]
      (cleanup-graph-nss!)
      (try
        (binding [*ns* *ns*]
          (f))
        (finally
          (in-ns (ns-name original-ns))
          (cleanup-graph-nss!))))))

(deftest create-formula-node-compile-error-test
  (let [graph {:graph/name "core-test-compile-error"
               :nodes [{:id "broken" :type :formula
                        :code "(fn [a] (+ a missing-symbol))"}]
               :edges []}
        runner (rc/create-runner {} graph)
        node (rc/get-node runner "broken")]
    (is (some? (:out node)))
    (let [values (m/? (m/reduce conj [] (:out node)))]
      (is (= 1 (count values)))
      (let [v (first values)]
        (is (rc/is-no-val? v))
        (is (instance? clojure.lang.ExceptionInfo (:ex v)))
        (is (re-find #"missing-symbol" (ex-message (:ex v))))))))

(deftest create-formula-node-unconnected-input-test
  (let [graph {:graph/name "core-test-unconnected-input"
               :nodes [{:id "missing" :type :formula
                        :code "(fn [ctx x] x)"}]
               :edges []}
        runner (rc/create-runner {} graph)
        node (rc/get-node runner "missing")]
    (is (some? (:out node)))
    (let [values (m/? (m/reduce conj [] (:out node)))]
      (is (= 1 (count values)))
      (let [v (first values)]
        (is (rc/is-no-val? v))
        (is (= "unconnected input: x" (:message-text v)))))))

(deftest create-formula-node-success-test
  (let [graph {:graph/name "core-test-success"
               :nodes [{:id "a" :type :value :value 15}
                       {:id "s" :type :formula :code "(fn [a] (+ a 1))"}]
               :edges [{:id "e-1" :source-node "a" :source-port "out"
                        :target-node "s" :target-port "a"}]}
        runner (rc/create-runner {} graph)]
    (is (= 16 (rc/get-snapshot runner "s")))))