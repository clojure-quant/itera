(ns itera.plot.text
  "Text viewer (pure hiccup, no JS).

   Renders a text string as hiccup, preserving newlines. itera has no
   Tailwind, so the inline monospace/pre styling is used while the optional
   `:class` is passed through to the outer element.

     (text {:text \"line one\\nline two\"})
     (text {:text \"hello\" :class \"my-text\" :style {:width \"50%\"}})

   The argument is a map with `:text` / `:class` / `:style` keys."
  (:require [clojure.string :as str]
            [itera.value.protocol]))

(def head [])

(defn- line-with-br [t]
  [:div
   [:span {:style {:font-family "monospace" :white-space "pre"}} t]
   [:br]])

(defn- default-style [{:keys [width] :as style}]
  (merge style {:width (or width "100%")}))

(defn- text->hiccup
  "Render `{:text <string> :class <string> :style <map>}` as hiccup,
   preserving newlines."
  ([text {:keys [class style]}]
   (into [:div]
         (map line-with-br
              (str/split (str (or text "<empty text>")) #"\n"))))
  ([text]
   (text->hiccup text {:style {:width "100%"}})))

(defrecord Text2 [text]
  itera.value.protocol/itera-convertable
  (to-hiccup [v]
    (text->hiccup (:text v))))


(defn text [text]
  (Text2. text))


(comment 
  (type (text "3"))
  (itera.value.protocol/to-hiccup (text "3\n5"))
 ; 
  )








