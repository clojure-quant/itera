(ns itera.model.ui-test
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [itera.model.ui :as rui]
   [itera.util.clj :as util]))

(defn- fresh-ctx []
  (let [path (str (java.nio.file.Files/createTempDirectory
                   "itera-ui-test"
                   (make-array java.nio.file.attribute.FileAttribute 0)))]
    {:itera/model-path path}))

(defn- model-file [ctx name]
  (io/file (:itera/model-path ctx) (str name ".edn")))

(defn- write-model! [ctx name nodes]
  (let [file (model-file ctx name)]
    (io/make-parents file)
    (spit file (pr-str {:graph/name name :nodes nodes :edges []}))
    file))

(defn- cleanup! [ctx name]
  (doseq [f (reverse (file-seq (io/file (:itera/model-path ctx))))]
    (io/delete-file f true))
  (let [ns-sym (symbol (str "itera.user." name))]
    (when (find-ns ns-sym)
      (remove-ns ns-sym))))

(defn- with-started [ctx name nodes f]
  (write-model! ctx name nodes)
  (let [this (rui/start-model ctx name)]
    (try
      (f this)
      (finally
        (rui/stop-model this)))))

(defn- value-node [id editor]
  {:id id :label id :type :value :value/editor editor
   :show? false :value {}})

(deftest normalize-requires-value-editor-test
  (let [ctx (fresh-ctx)
        missing "ui-test-missing-editor"
        bogus "ui-test-bogus-editor"]
    (try
      (testing "a :value node without :value/editor is rejected"
        (write-model! ctx missing [{:id "v" :label "no-editor" :type :value
                                    :show? false :value 1}])
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Invalid :value/editor"
                              (rui/start-model ctx missing))))
      (testing "an unsupported :value/editor is rejected"
        (write-model! ctx bogus [{:id "v" :type :value
                                  :value/editor :bogus
                                  :show? false :value 1}])
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Invalid :value/editor"
                              (rui/start-model ctx bogus))))
      (finally
        (cleanup! ctx missing)
        (cleanup! ctx bogus)))))

(deftest normalize-accepts-value-editors-test
  (let [ctx (fresh-ctx)
        name "ui-test-accepts-editors"]
    (try
      (with-started ctx name [(value-node "opt" :option-ui)
                              (value-node "edn" :edn)]
        (fn [this]
          (is (= :option-ui (:value/editor (rui/node-by-id this "opt"))))
          (is (= :edn (:value/editor (rui/node-by-id this "edn"))))))
      (finally
        (cleanup! ctx name)))))

(deftest add-node-value-editors-test
  (let [ctx (fresh-ctx)
        name "ui-test-add-node"]
    (try
      (with-started ctx name []
        (fn [this]
          (rui/add-node! this :value :option-ui)
          (rui/add-node! this :value :edn)
          (let [[opt edn] (:nodes @(:model-a this))]
            (testing ":option-ui value node defaults"
              (is (= :value (:type opt)))
              (is (= :option-ui (:value/editor opt)))
              (is (= {} (:value opt)))
              (is (= [] (:option-ui opt))))
            (testing ":edn value node defaults"
              (is (= :value (:type edn)))
              (is (= :edn (:value/editor edn)))
              (is (nil? (:value edn)))
              (is (not (contains? edn :option-ui)))))))
      (finally
        (cleanup! ctx name)))))

(deftest save-model-roundtrips-value-editor-test
  (let [ctx (fresh-ctx)
        name "ui-test-save-roundtrip"]
    (try
      (write-model! ctx name [(assoc (value-node "opt" :option-ui)
                                     :option-ui [{:path [:a] :type :string
                                                  :name "a" :spec ""}])
                              (value-node "edn" :edn)])
      (let [this (rui/start-model ctx name)]
        (try
          (rui/save-model-edn! this)
          (let [saved (edn/read-string (slurp (model-file ctx name)))
                saved-ns (:ns-data saved)
                by-id (into {} (map (juxt :id identity)) (:nodes saved))]
            (is (string? saved-ns))
            (is (= (rui/ns-data this) saved-ns))
            (is (= :option-ui (:value/editor (get by-id "opt"))))
            (is (= :edn (:value/editor (get by-id "edn"))))
            (rui/stop-model this)
            (let [reloaded (rui/start-model ctx name)]
              (try
                (is (= saved-ns (rui/ns-data reloaded)))
                (is (= :option-ui (:value/editor (rui/node-by-id reloaded "opt"))))
                (is (= :edn (:value/editor (rui/node-by-id reloaded "edn"))))
                (finally
                  (rui/stop-model reloaded)))))
          (finally
            (rui/stop-model this))))
      (finally
        (cleanup! ctx name)))))

(deftest start-model-seeds-generated-ns-data-test
  (let [ctx (fresh-ctx)
        name "ui-test-seeded-ns"]
    (try
      (with-started ctx name []
        (fn [this]
          (is (= (util/create-workbook-ns-code (str "itera.user." name) [])
                 (rui/ns-data this)))))
      (finally
        (cleanup! ctx name)))))

(deftest set-ns-data-evaluates-test
  (let [ctx (fresh-ctx)
        name "ui-test-set-ns"]
    (try
      (with-started ctx name []
        (fn [this]
          (let [ns-data (str "(ns itera.user.ui-test-set-ns"
                             " (:require [clojure.string :as str]))")
                result (rui/set-ns-data! this ns-data)]
            (is (nil? (:ex result)))
            (is (= ns-data (rui/ns-data this)))
            (is (= "ABC" (:value (util/compile-formula
                                  (:graph-ns this)
                                  "(str/upper-case \"abc\")")))))))
      (finally
        (cleanup! ctx name)))))

(deftest set-ns-data-returns-exception-test
  (let [ctx (fresh-ctx)
        name "ui-test-set-ns-error"]
    (try
      (with-started ctx name []
        (fn [this]
          (let [ns-data (str "(ns itera.user.ui-test-set-ns-error)\n"
                             "(throw (ex-info \"boom\" {}))")
                result (rui/set-ns-data! this ns-data)]
            (is (= "boom" (ex-message (:ex result))))
            (is (= ns-data (rui/ns-data this))))))
      (finally
        (cleanup! ctx name)))))

(deftest option-ui-nodes-filters-by-editor-test
  (let [ctx (fresh-ctx)
        name "ui-test-option-ui-nodes"
        rows [{:path [:a] :type :string :name "a" :spec ""}]]
    (try
      (with-started ctx name
        [(assoc (value-node "opt" :option-ui) :option-ui rows)
         (assoc (value-node "edn" :edn) :value nil :option-ui rows)]
        (fn [this]
          (is (= ["opt"] (mapv :node-id (rui/option-ui-nodes this))))))
      (finally
        (cleanup! ctx name)))))