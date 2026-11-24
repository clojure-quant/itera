(ns itera.plot.template-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [itera.plot.template :as template]
   [itera.value.protocol :as protocol]))

(defrecord Fake [data]
  protocol/itera-convertable
  (to-hiccup [_]
    [:fake-hiccup data])

  protocol/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    [:fake-formatted data spec window])
  (default-spec [_]
    {:fake :default})
  (spec-editor [_ spec code-a]
    [:fake-editor data spec code-a])

  protocol/itera-page-plot
  (to-formatted-paged-hiccup [_ spec window page]
    [:fake-paged data spec window page])
  (init-page [_ spec window]
    [:fake-init-page data spec window])
  (page-editor [_ spec window page-a]
    [:fake-page-editor data spec window page-a]))

(defmethod template/templateable-plot :fake [_] ->Fake)

(deftest draw-template-test
  (let [value (template/draw-template :fake "test-plot" [:row])]
    (is (instance? itera.plot.template.TemplatePlot value))
    (is (= {:panes []} (protocol/default-spec value)))
    (is (= [:fake-formatted [:row] {:panes []} nil]
           (protocol/to-hiccup value)))
    (is (= [:fake-formatted [:row] {:custom :spec} nil]
           (protocol/to-formatted-hiccup value {:custom :spec} nil)))
    (is (= [:fake-editor [:row] {:custom :spec} "(fn [])"]
           (protocol/spec-editor value {:custom :spec} "(fn [])")))))

(deftest page-delegation-test
  (let [value (template/draw-template :fake "test-plot" [:row])
        page {:page/current 1}]
    (is (= [:fake-paged [:row] {:custom :spec} nil page]
           (protocol/to-formatted-paged-hiccup value {:custom :spec} nil page)))
    (is (= [:fake-init-page [:row] {:custom :spec} nil]
           (protocol/init-page value {:custom :spec} nil)))
    (is (= [:fake-page-editor [:row] {:custom :spec} nil page]
           (protocol/page-editor value {:custom :spec} nil page)))))

(deftest missing-template-test
  (testing "missing resource"
    (is (thrown? clojure.lang.ExceptionInfo
                 (template/draw-template :fake "no-such-plot" [:row]))))
  (testing "unknown plot type"
    (is (thrown? IllegalArgumentException
                 (template/draw-template :unknown-plot-type "test-plot" [:row])))))