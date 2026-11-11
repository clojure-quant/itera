# Sample markdown resource

This document is loaded from a **classpath resource** (`demo/sample.md`) by
`itera.plot.md/md-resource`, then rendered to HTML by `commonmark-hiccup`.

## Lists

1. first
2. second
3. third

## Code

```clojure
(defn greet [name]
  (str "Hello, " name "!"))
```

## Quote

> The resource is read with `clojure.java.io/resource` and slurped.

A [link](https://clojure.org) for good measure.
