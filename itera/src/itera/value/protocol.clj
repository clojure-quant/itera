(ns itera.value.protocol)


(defprotocol itera-convertable
  (to-hiccup [v]))


(defprotocol itera-spec-plot
  (to-formatted-hiccup [v spec window])
  (default-spec [v])
  (spec-editor [v spec code-a]))


(defn window->style
  "CSS style from a container dimension {:width <px> :height <px>}."
  [window]
  (when window
    (cond-> {}
      (:width window)  (assoc :width  (str (:width window) "px"))
      (:height window) (assoc :height (str (:height window) "px")))))

(defprotocol itera-page-plot
  (to-formatted-paged-hiccup [data spec window page])
  (init-page [data spec window])
  (page-editor [data spec window page-a]))

(defprotocol itera-table-info
  "Table-shaped data access: a row count, a clamped row slice, per-column
   inferred types, and the rows as a sequence of maps."
  (total-rows [_]
    "Number of rows in the table.")
  (select-rows [_ idx-start idx-end]
    "Rows in the half-open index range [idx-start idx-end), clamped to the
     table bounds.  Returns a value of the same table shape.")
  (column-info [_]
    "Map of column name -> inferred type keyword
     (:number :string :boolean :keyword :time :nil :mixed :unknown).")
  (as-seq-of-maps [_]
    "The rows as a sequence of maps from column name to scalar value."))


(comment 
  (defrecord Hiccup [data]
    itera-spec-plot
    (to-formatted-hiccup [v spec container-dimension]
               {:data (:data v) :spec spec :container-dimension container-dimension})
    (default-spec [v] :default-spec)
    (spec-editor [v spec code-a]
      :hiccup-editor))
  
  (def x (Hiccup. :data))
  (to-formatted-hiccup x :rows :big)
  
 ; 
  )