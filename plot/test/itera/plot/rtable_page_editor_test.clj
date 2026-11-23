(ns itera.plot.rtable-page-editor-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [hyper.core :as h]
   [hyper.test :as ht]
   [itera.plot.rtable-page-editor :as page-editor]))

(defn- render-editor
  "Render the page editor for TOTAL pages and PAGE (nil -> no page cursor)."
  [total page]
  (ht/test-page
   (fn [_req]
     (let [page-a (when page (h/tab-cursor :page))]
       (page-editor/page-editor-view total page-a)))
   (cond-> {}
     page (assoc-in [:cursors :tab :page] page))))

(defn- elements [tag view]
  (filter #(and (vector? %) (= tag (first %)))
          (tree-seq coll? seq view)))

(defn- buttons [view]
  (elements :button view))

(defn- button-labels [view]
  (mapv #(:aria-label (second %)) (buttons view)))

(defn- button-by-label [view label]
  (first (filter #(= label (:aria-label (second %))) (buttons view))))

(defn- button-attrs [view label]
  (second (button-by-label view label)))

(defn- input-attrs [view]
  (second (first (elements :input view))))

(defn- text-children [view]
  (filter string? (flatten view)))

(def ^:private all-labels
  ["First page" "Previous page" "Next page" "Last page"])

(deftest nil-cursor-test
  (let [view  (:body (render-editor 3 nil))
        attrs (input-attrs view)]
    (testing "no page cursor renders all controls disabled"
      (is (= all-labels (button-labels view)))
      (doseq [label all-labels]
        (is (true? (:disabled (button-attrs view label))))
        (is (nil? (:data-on:click (button-attrs view label)))))
      (is (= "1" (:value attrs)))
      (is (true? (:disabled attrs)))
      (is (nil? (:data-on:keydown attrs)))
      (is (nil? (:data-on:blur attrs)))
      (is (some #{"/3"} (text-children view))))))

(deftest page-boundary-test
  (testing "page one of three enables only forward controls"
    (let [view (:body (render-editor 3 {:page/current 1 :page/total 3}))]
      (is (= all-labels (button-labels view)))
      (doseq [label ["First page" "Previous page"]]
        (is (true? (:disabled (button-attrs view label))))
        (is (nil? (:data-on:click (button-attrs view label)))))
      (doseq [label ["Next page" "Last page"]]
        (is (nil? (:disabled (button-attrs view label))))
        (is (some? (:data-on:click (button-attrs view label)))))
      (is (= "1" (:value (input-attrs view))))
      (is (some #{"/3"} (text-children view)))))
  (testing "a middle page enables every control"
    (let [view  (:body (render-editor 3 {:page/current 2 :page/total 3}))
          attrs (input-attrs view)]
      (is (= all-labels (button-labels view)))
      (doseq [label all-labels]
        (is (nil? (:disabled (button-attrs view label))))
        (is (some? (:data-on:click (button-attrs view label)))))
      (is (= "2" (:value attrs)))
      (is (some? (:data-on:keydown attrs)))
      (is (some? (:data-on:blur attrs)))
      (is (some #{"/3"} (text-children view)))))
  (testing "the last page enables only backward controls"
    (let [view (:body (render-editor 3 {:page/current 3 :page/total 3}))]
      (is (= all-labels (button-labels view)))
      (doseq [label ["First page" "Previous page"]]
        (is (nil? (:disabled (button-attrs view label))))
        (is (some? (:data-on:click (button-attrs view label)))))
      (doseq [label ["Next page" "Last page"]]
        (is (true? (:disabled (button-attrs view label))))
        (is (nil? (:data-on:click (button-attrs view label))))))))

(deftest single-page-test
  (let [view  (:body (render-editor 1 {:page/current 1 :page/total 1}))
        attrs (input-attrs view)]
    (testing "a single page disables all four buttons and the input"
      (is (= all-labels (button-labels view)))
      (doseq [label all-labels]
        (is (true? (:disabled (button-attrs view label)))))
      (is (= "1" (:value attrs)))
      (is (true? (:disabled attrs)))
      (is (nil? (:data-on:keydown attrs)))
      (is (nil? (:data-on:blur attrs)))
      (is (some #{"/1"} (text-children view))))))

(deftest out-of-range-display-test
  (testing "a current page above the last clamps in the displayed value"
    (is (= "3" (:value (input-attrs
                        (:body (render-editor 3 {:page/current 99
                                                 :page/total 3})))))))
  (testing "a current page below the first clamps in the displayed value"
    (is (= "1" (:value (input-attrs
                        (:body (render-editor 3 {:page/current 0
                                                 :page/total 3}))))))))

(defn- action-page [result action-name & [client-params]]
  (get-in (if client-params
            (ht/test-action result action-name client-params)
            (ht/test-action result action-name))
          [:cursors :tab :page :page/current]))

(deftest action-test
  (testing "first page jumps to one"
    (is (= 1 (action-page (render-editor 3 {:page/current 2 :page/total 3})
                          "first-page"))))
  (testing "previous page steps back one"
    (is (= 1 (action-page (render-editor 3 {:page/current 2 :page/total 3})
                          "previous-page"))))
  (testing "next page steps forward one"
    (is (= 3 (action-page (render-editor 3 {:page/current 2 :page/total 3})
                          "next-page"))))
  (testing "last page jumps to the total"
    (is (= 3 (action-page (render-editor 3 {:page/current 2 :page/total 3})
                          "last-page"))))
  (testing "jump clamps a value above the total"
    (is (= 3 (action-page (render-editor 3 {:page/current 2 :page/total 3})
                          "jump-to-page" {:value "99"}))))
  (testing "jump ignores non-numeric input"
    (is (= 2 (action-page (render-editor 3 {:page/current 2 :page/total 3})
                          "jump-to-page" {:value "abc"})))))