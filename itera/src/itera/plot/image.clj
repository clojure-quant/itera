(ns itera.plot.image
  "Image viewer: inline base64 images and cache-served images.

   The records store only the image (plus the image-cache id for cached
   images); display options are not stored at construction.  Both viewers
   implement itera.value.protocol/itera-spec-plot with the editable spec
   {:alt <string> :width <px> :height <px>}; `default-spec` uses the source
   image's natural pixel size (200x125 when dimensions are unavailable).  A
   blank `:width` or `:height` in the spec falls back to the natural size.

   `java.awt.image.BufferedImage` itself is extended to the cached-image spec
   plot, so raw images returned by model nodes render as cached plots without
   an `image` wrapper.  The image-cache id is assigned on first render and
   reused for the same instance."
  (:require [itera.model-ui.image-cache :as image-cache]
            [itera.plot.image-format :as image-format]
            [itera.value.protocol :as spec-plot])
  (:import [java.awt.image BufferedImage]
           [java.io ByteArrayOutputStream]
           [java.util Base64 Collections WeakHashMap]
           [javax.imageio ImageIO]))

(def head [])

(def fallback-size
  "Display size used when an image's natural pixel size is unavailable."
  [200 125])

(defn- natural-size
  "Source image's [width height] in pixels; nil when unavailable."
  [^BufferedImage image]
  (try [(.getWidth image) (.getHeight image)]
       (catch Exception _ nil)))

(defn- image-size [^BufferedImage image width height]
  (let [[iw ih] (or (natural-size image) fallback-size)]
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

(defn- inline-hiccup
  "Base64 <img> hiccup for IMAGE at the size in OPTS."
  [^BufferedImage image {:keys [alt type width height]
                         :or {alt "" type "png"}}]
  (let [[w h] (image-size image width height)
        bytes (image-to-bytes image type w h)
        src   (str "data:image/" type ";base64,"
                   (.encodeToString (Base64/getEncoder) bytes))]
    [:img {:src src :width w :height h :alt alt}]))

(defn- cached-hiccup
  "Cached <img> hiccup for IMAGE at the size in OPTS."
  [^BufferedImage image id {:keys [alt width height] :or {alt ""}}]
  (let [[w h] (image-size image width height)]
    [:img {:src    (str "/itera/image-cache/" id)
           :width  w
           :height h
           :alt    alt}]))

(defn- natural-default-spec [^BufferedImage image]
  (let [[w h] (or (natural-size image) fallback-size)]
    {:alt "" :width w :height h}))

(defonce ^:private image-ids
  ;; Weak keys: the cache itself owns the encoded bytes; the mapping only makes
  ;; repeated renders of the same image instance reuse one cache id.
  (Collections/synchronizedMap (WeakHashMap.)))

(defn- cached-id
  "Stable cache id for IMAGE, registering it on first use."
  [^BufferedImage image]
  (or (.get image-ids image)
      (let [id (image-cache/add-cache-image image)]
        (locking image-ids
          (or (.get image-ids image)
              (do (.put image-ids image id)
                  id))))))

(defrecord ImageInline [image]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] (inline-hiccup image {}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec _window] (inline-hiccup image spec))
  (default-spec [_] (natural-default-spec image))
  (spec-editor [_ spec code-a]
    (image-format/image-format-editor spec code-a)))

(defrecord Image [image id]
  itera.value.protocol/itera-convertable
  (to-hiccup [_] (cached-hiccup image id {}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec _window] (cached-hiccup image id spec))
  (default-spec [_] (natural-default-spec image))
  (spec-editor [_ spec code-a]
    (image-format/image-format-editor spec code-a)))

(defn image-inline
  "Return an inline (base64 `data:` URL) image.

   The value stores only the image; its editable spec
   `{:alt ... :width ... :height ...}` controls the display.  Width/height
   fall back to the natural size when absent, and aspect ratio is preserved
   when only one is given.  The spec may also carry `:type` (default `\"png\"`)."
  [^BufferedImage image]
  (ImageInline. image))

(defn image
  "Return an image served from the in-memory cache at
   `/itera/image-cache/:image-id`.

   The value stores only the image and the cache id; its editable spec
   `{:alt ... :width ... :height ...}` controls the display.  Width/height
   fall back to the natural size when absent, and aspect ratio is preserved
   when only one is given.  The cached bytes are always PNG."
  [^BufferedImage image]
  (Image. image (cached-id image)))

(extend-type BufferedImage
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [^BufferedImage image spec _window]
    (cached-hiccup image (cached-id image) spec))
  (default-spec [^BufferedImage image]
    (natural-default-spec image))
  (spec-editor [_image spec code-a]
    (image-format/image-format-editor spec code-a)))
