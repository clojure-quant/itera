(ns itera.model-ui.image-cache
  "In-memory image cache served at /itera/image-cache/:image-id."
  (:require [nano-id.core :as nano-id])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream]
           [javax.imageio ImageIO]))

(def image-cache-a (atom {}))

(defn- png-bytes [^BufferedImage image]
  (let [baos (ByteArrayOutputStream.)]
    (ImageIO/write image "png" baos)
    (.toByteArray baos)))

(defn add-cache-image
  "Cache `image` and return a nano-id usable as its URL id."
  [image]
  (let [id (nano-id/nano-id 16)]
    (swap! image-cache-a assoc id {:bytes (png-bytes image)
                                   :content-type "image/png"})
    id))

(defn image-handler
  "Ring handler for GET /itera/image-cache/:image-id."
  [req]
  (let [id (get-in req [:path-params :image-id])]
    (if-let [{:keys [bytes content-type]} (get @image-cache-a id)]
      {:status 200
       :headers {"Content-Type" content-type
                 "Cache-Control" "public, max-age=31536000, immutable"}
       :body bytes}
      {:status 404
       :headers {"Content-Type" "text/plain; charset=utf-8"}
       :body (str "Unknown image id: " id)})))