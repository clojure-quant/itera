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
        value (pimg/image-inline img)
        hiccup (protocol/to-hiccup value)
        [tag attrs] hiccup]
    (is (= :img tag))
    (is (= "" (:alt attrs)))
    (is (= 4 (:width attrs)))
    (is (= 2 (:height attrs)))
    (is (str/starts-with? (:src attrs) "data:image/png;base64,"))))

(deftest image-inline-width-test
  (let [attrs (second (protocol/to-formatted-hiccup
                       (pimg/image-inline (test-image)) {:width 2} nil))]
    (is (= 2 (:width attrs)))
    (is (= 1 (:height attrs)))))

(deftest image-inline-type-test
  (let [attrs (second (protocol/to-formatted-hiccup
                       (pimg/image-inline (test-image)) {:type "gif"} nil))]
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
  (let [attrs (second (protocol/to-hiccup (pimg/image (test-image))))
        id (last (str/split (:src attrs) #"/"))]
    (try
      (is (= "" (:alt attrs)))
      (is (= 4 (:width attrs)))
      (is (= 2 (:height attrs)))
      (is (str/starts-with? (:src attrs) "/itera/image-cache/"))
      (is (contains? @image-cache/image-cache-a id))
      (is (= 200 (:status (image-cache/image-handler
                           {:path-params {:image-id id}}))))
      (finally
        (swap! image-cache/image-cache-a dissoc id)))))

(deftest image-spec-plot-test
  (let [inline (pimg/image-inline (test-image))
        cached (pimg/image (test-image))
        id     (:id cached)]
    (try
      (is (satisfies? protocol/itera-spec-plot inline))
      (is (satisfies? protocol/itera-spec-plot cached))
      (finally
        (swap! image-cache/image-cache-a dissoc id)))))

(deftest image-default-spec-test
  (let [value (pimg/image-inline (test-image))]
    (is (= {:alt "" :width 4 :height 2}
           (protocol/default-spec value)))))

(deftest image-formatted-hiccup-test
  (let [value (pimg/image-inline (test-image))
        [tag attrs] (protocol/to-formatted-hiccup
                     value {:alt "z" :width 2 :height 1} nil)]
    (is (= :img tag))
    (is (= "z" (:alt attrs)))
    (is (= 2 (:width attrs)))
    (is (= 1 (:height attrs)))))

(deftest image-formatted-hiccup-keeps-type-test
  (let [value (pimg/image-inline (test-image))
        [_ attrs] (protocol/to-formatted-hiccup
                   value {:type "gif" :width 2 :height 1} nil)]
    (is (str/starts-with? (:src attrs) "data:image/gif;base64,"))))

(deftest image-cached-formatted-hiccup-test
  (let [value (pimg/image (test-image))
        [tag default-attrs] (protocol/to-hiccup value)
        id (:id value)]
    (try
      (is (= :img tag))
      (is (= (str "/itera/image-cache/" id) (:src default-attrs)))
      (let [before (count @image-cache/image-cache-a)
            [_ attrs] (protocol/to-formatted-hiccup
                       value {:alt "z" :width 2 :height 1} nil)]
        (is (= "z" (:alt attrs)))
        (is (= 2 (:width attrs)))
        (is (= 1 (:height attrs)))
        (is (= (str "/itera/image-cache/" id) (:src attrs)))
        (is (= before (count @image-cache/image-cache-a)))
        (protocol/to-formatted-hiccup value {:width 3} nil)
        (is (= before (count @image-cache/image-cache-a)))
        (is (contains? @image-cache/image-cache-a id)))
      (finally
        (swap! image-cache/image-cache-a dissoc id)))))

(deftest image-default-spec-fallback-test
  (is (= {:alt "" :width 200 :height 125}
         (protocol/default-spec (pimg/->ImageInline nil))))
  (is (= {:alt "" :width 200 :height 125}
         (protocol/default-spec (pimg/->Image nil nil)))))

(deftest buffered-image-spec-plot-test
  (let [img (test-image)]
    (is (satisfies? protocol/itera-spec-plot img))
    (is (= {:alt "" :width 4 :height 2}
           (protocol/default-spec img)))))

(deftest buffered-image-formatted-hiccup-test
  (let [img (test-image)
        [tag default-attrs] (protocol/to-formatted-hiccup img {} nil)
        id (last (str/split (:src default-attrs) #"/"))]
    (try
      (is (= :img tag))
      (is (= "" (:alt default-attrs)))
      (is (= 4 (:width default-attrs)))
      (is (= 2 (:height default-attrs)))
      (is (str/starts-with? (:src default-attrs) "/itera/image-cache/"))
      (is (= 200 (:status (image-cache/image-handler
                           {:path-params {:image-id id}}))))
      (let [before (count @image-cache/image-cache-a)
            [_ attrs] (protocol/to-formatted-hiccup
                       img {:alt "z" :width 2 :height 1} nil)]
        (is (= "z" (:alt attrs)))
        (is (= 2 (:width attrs)))
        (is (= 1 (:height attrs)))
        (is (= (:src default-attrs) (:src attrs)))
        (is (= before (count @image-cache/image-cache-a))))
      (finally
        (swap! image-cache/image-cache-a dissoc id)))))

(deftest image-and-buffered-image-share-cache-test
  (let [img (test-image)
        value (pimg/image img)
        id (:id value)]
    (try
      (let [[_ record-attrs] (protocol/to-formatted-hiccup value {} nil)
            [_ raw-attrs] (protocol/to-formatted-hiccup img {} nil)]
        (is (= (:src record-attrs) (:src raw-attrs)))
        (is (= (str "/itera/image-cache/" id) (:src raw-attrs))))
      (finally
        (swap! image-cache/image-cache-a dissoc id)))))
