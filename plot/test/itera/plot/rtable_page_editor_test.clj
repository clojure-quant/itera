(ns itera.plot.rtable-page-editor-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [hyper.core :as h]
   [hyper.test :as ht]
   [itera.plot.rtable :as rtable]
   [itera.plot.rtable-page-editor :as page-editor]))

(defn- render-editor
  "Render the page editor for PAGE (nil -> no page cursor)."
  [v page]
  (ht/test-page
   (fn [_req]
     (let [page-a (when page (h/tab-cursor :page))]
       (page-editor/page-editor-view v page-a)))
   (cond-> {}
     page (assoc-in [:cursors :tab :page] page))))

(defn- elements [tag view]
  (filter #(and (vector? %) (= tag (first %)))
          (tree-seq coll? seq view)))

(defn- buttons [view]
  (elements :button view))

(defn- button-labels [view]
  (mapv #(:aria-label (second %)) (buttons view)))

(defn- input-attrs [view]
  (second (first (elements :input view))))

(defn- text-children [view]
  (filter string? (flatten view)))

(deftest nil-cursor-test
  (let [v    (rtable/rtable (mapv (fn [i] {:a i}) (range 25)))
        view (:body (render-editor v nil))
        attrs (input-attrs view)]
    (testing "no page cursor renders a disabled box and no buttons"
      (is (empty? (buttons view)))
      (is (= "1" (:value attrs)))
      (is (true? (:disabled attrs)))
      (is (nil? (:data-on:keydown attrs)))
      (is (nil? (:data-on:blur attrs))))))

(deftest page-boundary-test
  (testing "page one of three only shows forward controls"
    (let [view (:body (render-editor nil {:page/current 1 :page/total 3}))]
      (is (= ["Next page" "Last page"] (button-labels view)))
      (is (= "1" (:value (input-attrs view))))
      (is (some #{"/3"} (text-children view)))))
  (testing "a middle page shows all four controls and an editable box"
    (let [view  (:body (render-editor nil {:page/current 2 :page/total 3}))
          attrs (input-attrs view)]
      (is (= ["First page" "Previous page" "Next page" "Last page"]
             (button-labels view)))
      (is (= "2" (:value attrs)))
      (is (some? (:data-on:keydown attrs)))
      (is (some? (:data-on:blur attrs)))
      (is (some #{"/3"} (text-children view)))))
  (testing "the last page only shows backward controls"
    (let [view (:body (render-editor nil {:page/current 3 :page/total 3}))]
      (is (= ["First page" "Previous page"] (button-labels view))))))

(deftest out-of-range-display-test
  (testing "a current page above the last clamps in the displayed value"
    (is (= "3" (:value (input-attrs
                        (:body (render-editor nil {:page/current 99
                                                   :page/total 3})))))))
  (testing "a current page below the first clamps in the displayed value"
    (is (= "1" (:value (input-attrs
                        (:body (render-editor nil {:page/current 0
                                                   :page/total 3}))))))))

(defn- action-page [result action-name & [client-params]]
  (get-in (if client-params
            (ht/test-action result action-name client-params)
            (ht/test-action result action-name))
          [:cursors :tab :page :page/current]))

(deftest action-test
  (testing "first page jumps to one"
    (is (= 1 (action-page (render-editor nil {:page/current 2 :page/total 3})
                          "first-page"))))
  (testing "previous page steps back one"
    (is (= 1 (action-page (render-editor nil {:page/current 2 :page/total 3})
                          "previous-page"))))
  (testing "next page steps forward one"
    (is (= 3 (action-page (render-editor nil {:page/current 2 :page/total 3})
                          "next-page"))))
  (testing "last page jumps to the total"
    (is (= 3 (action-page (render-editor nil {:page/current 2 :page/total 3})
                          "last-page"))))
  (testing "jump clamps a value above the total"
    (is (= 3 (action-page (render-editor nil {:page/current 2 :page/total 3})
                          "jump-to-page" {:value "99"}))))
  (testing "jump ignores non-numeric input"
    (is (= 2 (action-page (render-editor nil {:page/current 2 :page/total 3})
                          "jump-to-page" {:value "abc"})))))