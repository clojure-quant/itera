(ns itera.plot.image-test
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is]]
   [itera.model-ui.image-cache :as image-cache]
   [itera.plot.image :as pimg]
   [itera.value.protocol :as protocol])
  (:import
   [java.awt.image BufferedImage]))

(defn- test-image []
  (BufferedImage. 4 2 BufferedImage/TYPE_INT_ARGB))

(deftest image-inline-test
  (let [img (test-image)
        value (pimg/image-inline img {:alt "x"})
        hiccup (protocol/to-hiccup value)
        [tag attrs] hiccup]
    (is (= :img tag))
    (is (= "x" (:alt attrs)))
    (is (= 4 (:width attrs)))
    (is (= 2 (:height attrs)))
    (is (str/starts-with? (:src attrs) "data:image/png;base64,"))))

(deftest image-inline-width-test
  (let [attrs (second (protocol/to-hiccup
                       (pimg/image-inline (test-image) {:width 2})))]
    (is (= 2 (:width attrs)))
    (is (= 1 (:height attrs)))))

(deftest image-inline-type-test
  (let [attrs (second (protocol/to-hiccup
                       (pimg/image-inline (test-image) {:type "gif"})))]
    (is (str/starts-with? (:src attrs) "data:image/gif;base64,"))))

(deftest image-cache-test
  (let [img (test-image)
        id (image-cache/add-cache-image img)]
    (try
      (is (string? id))
      (is (= "image/png"
             (get-in @image-cache/image-cache-a [id :content-type])))
      (let [resp (image-cache/image-handler {:path-params {:image-id id}})]
        (is (= 200 (:status resp)))
        (is (= "image/png" (get-in resp [:headers "Content-Type"])))
        (is (pos? (alength ^bytes (:body resp)))))
      (finally
        (swap! image-cache/image-cache-a dissoc id)))))

(deftest image-cache-unknown-id-test
  (let [resp (image-cache/image-handler {:path-params {:image-id "unknown-id"}})]
    (is (= 404 (:status resp)))
    (is (= "Unknown image id: unknown-id" (:body resp)))))

(deftest image-test
  (let [attrs (second (protocol/to-hiccup
                       (pimg/image {:alt "cached" :width 2} (test-image))))
        id (last (str/split (:src attrs) #"/"))]
    (try
      (is (= "cached" (:alt attrs)))
      (is (= 2 (:width attrs)))
      (is (= 1 (:height attrs)))
      (is (str/starts-with? (:src attrs) "/itera/image-cache/"))
      (is (contains? @image-cache/image-cache-a id))
      (is (= 200 (:status (image-cache/image-handler
                           {:path-params {:image-id id}}))))
      (finally
        (swap! image-cache/image-cache-a dissoc id)))))