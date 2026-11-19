(ns itera.plot.image
  "Image viewer: inline base64 images and cache-served images."
  (:require [itera.model-ui.image-cache :as image-cache]
            [itera.value.protocol])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream]
           [java.util Base64]
           [javax.imageio ImageIO]))

(def head [])

(defn- image-size [^BufferedImage image width height]
  (let [iw (.getWidth image)
        ih (.getHeight image)]
    (cond
      (and width height) [(int width) (int height)]
      width              [(int width) (int (* (/ width iw) ih))]
      height             [(int (* (/ height ih) iw)) (int height)]
      :else              [iw ih])))

(defn- image-to-bytes [^java.awt.Image image type width height]
  (let [bi   (BufferedImage. width height
                             (if (#{"png" "gif"} type)
                               BufferedImage/TYPE_INT_ARGB
                               BufferedImage/TYPE_INT_RGB))
        g    (.getGraphics bi)
        baos (ByteArrayOutputStream.)]
    (.drawImage g image 0 0 width height nil)
    (.dispose g)
    (ImageIO/write bi type baos)
    (.toByteArray baos)))

(defrecord ImageInline [hiccup]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] hiccup))

(defrecord Image [hiccup]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] hiccup))

(defn image-inline
  "Return an inline (base64 `data:` URL) image, optionally scaled.

   `opts` keys: `:alt` (default `\"\"`), `:type` (default `\"png\"`),
   `:width` and `:height` (aspect ratio is preserved when only one is given)."
  [^BufferedImage image {:keys [alt type width height]
                         :or {alt "" type "png"}}]
  (let [[w h] (image-size image width height)
        bytes (image-to-bytes image type w h)
        src   (str "data:image/" type ";base64,"
                   (.encodeToString (Base64/getEncoder) bytes))]
    (ImageInline. [:img {:src src :width w :height h :alt alt}])))

(defn image
  "Return an image served from the in-memory cache at
   `/itera/image-cache/:image-id`, optionally scaled.

   `opts` keys: `:alt` (default `\"\"`), `:width` and `:height` (aspect ratio is
   preserved when only one is given).  The cached bytes are always PNG."
  [{:keys [alt width height] :or {alt ""}} ^BufferedImage image]
  (let [[w h] (image-size image width height)
        id    (image-cache/add-cache-image image)]
    (Image. [:img {:src    (str "/itera/image-cache/" id)
                   :width  w
                   :height h
                   :alt    alt}])))