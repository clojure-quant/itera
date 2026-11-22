(ns itera.plot.image-format
  "Spec editor for itera.plot.image viewers.

   Edits the map spec {:alt <string> :width <px> :height <px>}.  Alt text is
   stored as-is (blank is allowed); width and height accept positive whole
   numbers, a blank input removes the key so rendering falls back to the
   image's natural size, and invalid input leaves the spec unchanged.  Keys
   outside :alt / :width / :height are preserved.

   Every edit resets the caller's code cursor to (pr-str spec); the existing
   spec popup persists it when it closes.  Must be called inside a Hyper
   render context."
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [hyper.core :as h]))

(defn- read-spec
  "Current spec map: parsed from code-a when it holds a valid EDN map, else
   SPEC when it is a map, else {}."
  [spec code-a]
  (let [text   (try (when code-a @code-a) (catch Exception _ nil))
        parsed (try
                 (when (and (string? text) (not (str/blank? text)))
                   (edn/read-string text))
                 (catch Exception _ nil))]
    (cond
      (map? parsed) parsed
      (map? spec)   spec
      :else         {})))

(defn- write-spec!
  [code-a spec]
  (reset! code-a (pr-str spec)))

(defn- parse-dimension
  "Positive whole number from text; nil when the text is blank, not numeric,
   or not a positive integer."
  [s]
  (when-not (str/blank? s)
    (try
      (let [d (parse-double s)]
        (when (and d
                   (not (Double/isNaN d))
                   (not (Double/isInfinite d))
                   (== d (Math/rint d))
                   (pos? d))
          (long d)))
      (catch Exception _ nil))))

(defn- set-alt!
  [spec code-a value]
  (write-spec! code-a (assoc (read-spec spec code-a) :alt (or value ""))))

(defn- set-dimension!
  [spec code-a k value]
  (if (str/blank? value)
    (write-spec! code-a (dissoc (read-spec spec code-a) k))
    (when-let [n (parse-dimension value)]
      (write-spec! code-a (assoc (read-spec spec code-a) k n)))))

(def ^:private row-style
  {:display "flex" :align-items "center" :gap "0.5rem"})

(def ^:private label-style
  {:width "3.5rem" :flex "0 0 3.5rem" :color "#374151"})

(def ^:private input-style
  {:box-sizing "border-box" :padding "0.25rem 0.4rem"
   :border "1px solid #d1d5db" :border-radius "0.375rem"
   :font-size "0.8rem"})

(defn image-format-editor
  "Edit an image spec {:alt ... :width ... :height ...} bound to code-a.
   Must be called inside a Hyper render context."
  [spec code-a]
  (let [current (read-spec spec code-a)]
    [:div {:style {:display "flex" :flex-direction "column" :gap "0.5rem"
                   :font-family "system-ui, sans-serif" :font-size "0.8rem"}}
     [:label {:style row-style}
      [:span {:style label-style} "Alt"]
      [:input {:type "text"
               :aria-label "Alt text"
               :value (or (:alt current) "")
               :style (assoc input-style :flex "1" :min-width "0")
               :data-on:change (h/action (set-alt! spec code-a $value))}]]

     [:label {:style row-style}
      [:span {:style label-style} "Width"]
      [:input {:type "number" :min "1" :step "1"
               :aria-label "Width"
               :placeholder "natural"
               :title "Pixel width; leave blank for the natural size"
               :value (if (some? (:width current)) (str (:width current)) "")
               :style (assoc input-style :flex "1" :min-width "0")
               :data-on:change
               (h/action (set-dimension! spec code-a :width $value))}]]

     [:label {:style row-style}
      [:span {:style label-style} "Height"]
      [:input {:type "number" :min "1" :step "1"
               :aria-label "Height"
               :placeholder "natural"
               :title "Pixel height; leave blank for the natural size"
               :value (if (some? (:height current)) (str (:height current)) "")
               :style (assoc input-style :flex "1" :min-width "0")
               :data-on:change
               (h/action (set-dimension! spec code-a :height $value))}]]

     [:div {:style {:color "#6b7280" :font-size "0.7rem"}}
      "Blank width or height restores the image's natural size."]]))