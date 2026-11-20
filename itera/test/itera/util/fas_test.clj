(ns itera.util.fas-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [hyper.core :as h]
   [itera.util.fas :as fas]))

(deftest icon-class-test
  (testing "keyword and string input, with or without the fa- prefix"
    (is (= "fa-solid fa-user-nurse" (fas/icon-class :fa-user-nurse)))
    (is (= "fa-solid fa-user-nurse" (fas/icon-class :user-nurse)))
    (is (= "fa-solid fa-user-nurse" (fas/icon-class "fa-user-nurse")))
    (is (= "fa-solid fa-user-nurse" (fas/icon-class "user-nurse")))))

(deftest fas-test
  (testing "renders the icon and marks the tab's CSS as loaded"
    (let [loaded (atom false)]
      (with-redefs [h/tab-cursor (fn [& _] loaded)]
        (is (= [:i {:class "fa-solid fa-user-nurse" :aria-hidden "true"}]
               (fas/fas :fa-user-nurse)))
        (is (true? @loaded)))))

  (testing "a label makes the icon accessible instead of hidden"
    (let [loaded (atom false)]
      (with-redefs [h/tab-cursor (fn [& _] loaded)]
        (is (= [:i {:class "fa-solid fa-user-nurse"
                    :role "img"
                    :aria-label "Nurse"}]
               (fas/fas :fa-user-nurse {:label "Nurse"}))))))

  (testing "extra classes and pass-through attributes"
    (let [loaded (atom false)]
      (with-redefs [h/tab-cursor (fn [& _] loaded)]
        (is (= [:i {:class "fa-solid fa-user-nurse extra big"
                    :aria-hidden "true"
                    :style {:color "red"}
                    :title "Nurse"
                    :id "nurse-icon"}]
               (fas/fas :fa-user-nurse {:class ["extra" "big"]
                                        :style {:color "red"}
                                        :title "Nurse"
                                        :id "nurse-icon"})))))))

(deftest head-test
  (testing "no link until the tab has used an icon"
    (let [loaded (atom false)]
      (with-redefs [h/tab-cursor (fn [& _] loaded)]
        (is (nil? (fas/head {})))
        (reset! loaded true)
        (is (= [[:link {:rel "stylesheet" :href fas/css-href}]]
               (fas/head {}))))))

  (testing "repeated icon use still contributes exactly one link"
    (let [loaded (atom false)]
      (with-redefs [h/tab-cursor (fn [& _] loaded)]
        (fas/fas :fa-user)
        (fas/fas :fa-user)
        (is (= [[:link {:rel "stylesheet" :href fas/css-href}]]
               (fas/head {})))))))