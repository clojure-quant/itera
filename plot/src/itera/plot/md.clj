(ns itera.plot.md
  "Markdown viewer (pure hiccup, no JS).

   The markdown source is stored together with the `:class` / `:style`
   container passthrough; conversion to HTML with commonmark-hiccup happens
   lazily in `to-hiccup`, embedded verbatim inside an `.itera-md` container
   whose typography comes from the stylesheet exposed via `head`.

   `md` takes markdown given as a string; `md-resource` reads it from a
   classpath resource first:

     (md \"# Title\")
     (md \"# Title\" {:class \"my-md\" :style {:max-width \"40rem\"}})
     (md-resource \"demo/sample.md\")

   The optional opts map only carries `:class` / `:style` container
   passthrough; the first argument is the markdown source (a string) or a
   resource path."
  (:require [clojure.java.io :as io]
            [commonmark-hiccup.core :as commonmark]
            [dev.onionpancakes.chassis.core :as chassis]
            [itera.value.protocol]))

(def head
  [[:style
    ".itera-md { max-width: 60rem; line-height: 1.5; }
     .itera-md h1, .itera-md h2, .itera-md h3 { margin: 1rem 0 0.5rem; }
     .itera-md pre { background: #f6f6f6; padding: 0.75rem; overflow: auto; border-radius: 0.375rem; }
     .itera-md code { background: #f0f0f0; padding: 0 0.25rem; border-radius: 0.25rem; }
     .itera-md pre code { background: none; padding: 0; }
     .itera-md blockquote { margin-left: 0; padding-left: 0.75rem; border-left: 4px solid #ccc; color: #555; }
     .itera-md table { border-collapse: collapse; }
     .itera-md th, .itera-md td { border: 1px solid #ccc; padding: 0.25rem 0.6rem; }"]])

(defn- container-attrs [class style]
  (cond-> {:class "itera-md"}
    class (update :class #(str % " " class))
    style (assoc :style style)))

(defn render
  "Render markdown source to the `itera-md` container hiccup."
  [markdown {:keys [class style]}]
  [:div (container-attrs class style)
   (chassis/raw (commonmark/markdown->html (str (or markdown ""))))])

(defrecord Md [markdown class style]
  itera.value.protocol/itera-convertable
  (to-hiccup [_]
    (render markdown {:class class :style style})))

(defn md
  "Render markdown given as a string to a hiccup tree wrapped in an
   `itera-convertable` record that renders the source lazily.

   Optional opts map: `{:class <string> :style <map>}` container passthrough."
  ([markdown] (md markdown nil))
  ([markdown opts]
   (let [{:keys [class style]} opts]
     (Md. (str (or markdown "")) class style))))

(defn md-resource
  "Render markdown read from a classpath resource to a hiccup tree wrapped in
   an `itera-convertable` record that renders the source lazily.

   Optional opts map: `{:class <string> :style <map>}` container passthrough.
   A missing resource renders an inline notice instead of throwing."
  ([resource] (md-resource resource nil))
  ([resource {:keys [class style]}]
   (if-let [r (io/resource resource)]
     (Md. (slurp r :encoding "UTF-8") class style)
     (Md. (str "Markdown resource not found: " resource) class style))))