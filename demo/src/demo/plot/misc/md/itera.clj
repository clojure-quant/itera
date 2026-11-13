(ns demo.plot.misc.md.itera
  (:require [itera.plot.md :as md]))

(def markdown
  "# Markdown from a string

This page uses `itera.plot.md/md` to render markdown that lives **in code**.

## Lists

- one
- two
- three

## Code

```clojure
(defn greet [name]
  (str \"Hello, \" name \"!\"))
```

> A quote from the string source.

---

A [link](https://clojure.org) and some `inline code`."
)

(defn calculate [_ctx _opts]
  {:markdown markdown
   :resource "demo/sample.md"})

(defn plot [_opts {:keys [markdown resource]}]
  [:div
   [:h2 "md — markdown from a string"]
   (md/md markdown)
   [:h2 "md-resource — markdown from a resource file"]
   (md/md-resource resource)])

(def algo-md
  {:calculate/fn calculate
   :plot/fn      plot})
