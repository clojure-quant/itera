(ns itera.plot.template
  "Render a plot with a predefined spec loaded from a classpath resource.

   A template lives at
   itera/template/<plot-type-name>/<template-name>.edn and contains the value
   that a model stores under :spec for that plot type.

   `templateable-plot` dispatches on the plot-type keyword and returns the
   base plot's constructor (data -> plot record). Each plot namespace registers
   its own method."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [itera.value.protocol :as protocol]))

(defmulti templateable-plot
  "Plot-type keyword -> base-plot constructor (fn [data] ->record)."
  (fn [plot-type] plot-type))

(defn template
  "Base-plot constructor for PLOT-TYPE."
  [plot-type]
  (templateable-plot plot-type))

(defn load-from-resources
  "Read and parse the EDN classpath resource at PATH. Throws when missing."
  [path]
  (if-let [r (io/resource path)]
    (edn/read-string (slurp r :encoding "UTF-8"))
    (throw (ex-info (str "template resource not found: " path)
                    {:resource path}))))

(defrecord TemplatePlot [base template data]
  protocol/itera-convertable
  (to-hiccup [_]
    (protocol/to-formatted-hiccup base template nil))

  protocol/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (protocol/to-formatted-hiccup base spec window))
  (default-spec [_] template)
  (spec-editor [_ spec code-a]
    (protocol/spec-editor base spec code-a))

  protocol/itera-page-plot
  (to-formatted-paged-hiccup [_ spec window page]
    (if (satisfies? protocol/itera-page-plot base)
      (protocol/to-formatted-paged-hiccup base spec window page)
      (protocol/to-formatted-hiccup base spec window)))
  (init-page [_ spec window]
    (when (satisfies? protocol/itera-page-plot base)
      (protocol/init-page base spec window)))
  (page-editor [_ spec window page-a]
    (when (satisfies? protocol/itera-page-plot base)
      (protocol/page-editor base spec window page-a))))

(defn draw-template
  "Wrap DATA in the plot for PLOT-TYPE with the spec stored at
   itera/template/<plot-type-name>/<template-name>.edn."
  [plot-type template-name data]
  (let [base     ((template plot-type) data)
        path     (str "itera/template/" (name plot-type) "/" template-name ".edn")
        template (load-from-resources path)]
    (->TemplatePlot base template data)))