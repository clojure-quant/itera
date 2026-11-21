(ns itera.plot.rtable
  "Server-rendered HTML table viewer (pure hiccup, no JS).

   The public `rtable` fn takes only the row `data`:
     (rtable [{...} ...])

   The editable spec is a map:
     {:row-height \"1.8rem\"
      :columns [{:path :close :header \"Close\"} ...]}

   `:row-height` is a CSS length applied to every body <tr>; it defaults to
   itera.plot.rtable-format-ui/default-row-height.  `:columns` definitions
   mirror the dali-table `rtable` viewer's server-expressible parts: column
   `:path` / `:header` / `:attrs`, `:class` / `:style` / `:max-width` /
   `:width` styling, and formatting through `:format` / `:format-opts` /
   `:style-fn`.  The format and style keywords dispatch the multimethods in
   itera.plot.rtable-format-ui; the built-in format methods are registered by
   itera.plot.rtable-default-column-formatter.  A legacy fn `:format` and fn
   `:attrs` are still applied directly.

   Legacy column vectors are not accepted: a non-map spec, or a map without a
   vector :columns, renders an error block.

   With no spec given, columns are derived from the first row's keys (the
   header via the existing `header-label`).  The spec editor is a table UI
   (itera.plot.rtable-format-ui/rtable-format-edit-ui) that writes plain EDN,
   so custom format/style methods must be registered by keyword.

   The returned `Rtable` record implements both itera.value.protocol (renders
   the default table) and itera.plot-ui.protocol (`:columns` as the editable
   spec)."
  (:require [clojure.string :as str]
            [itera.plot.rtable-default-column-formatter :as rtable-default-fmt]
            [itera.plot.rtable-format-ui :as rtable-fmt]
            [itera.plot-ui.protocol :as spec-plot]
            [itera.value.protocol]))

(def head
  [[:link {:rel  "stylesheet"
           :href "/itera/plot/rtable/rtable.css"}]])

(def default-class
  "Default table classes: compact padding, striping and row hover."
  "padding-sm table-striped table-hover")

(defn- col-value
  "Resolve a row's value for a column via its :path (keyword/vector) or :expr."
  [row {:keys [path expr]}]
  (cond
    path (if (vector? path) (get-in row path) (get row path))
    expr (expr row)
    :else nil))

(defn- format-value
  "Format VALUE for a column: a legacy fn :format is applied directly, a
   keyword dispatches format-rtable-col, otherwise default-format is used."
  [col value]
  (let [fmt (:format col)]
    (cond
      (fn? fmt)      (fmt value)
      (keyword? fmt) (rtable-fmt/format-rtable-col fmt (:format-opts col) value)
      :else          (rtable-default-fmt/default-format value))))

(defn- header-label [{:keys [header path]}]
  (or header
      (cond
        (keyword? path) (name path)
        path            (str path)
        :else           "")))

(defn- default-columns
  "Columns derived from the first row's keys; the header comes from
   header-label.  nil when there are no rows."
  [rows]
  (when-let [row (first rows)]
    (mapv (fn [k] {:path k}) (keys row))))

(defn- max-width-style [max-width]
  (when max-width
    {:max-width    max-width
     :overflow     "hidden"
     :text-overflow "ellipsis"
     :white-space  "nowrap"}))

(defn- width-style [width]
  (when (some? width)
    {:width (if (number? width) (str width "px") width)}))

(defn- cell-attrs [{:keys [class style max-width width]}]
  (let [style (merge (max-width-style max-width) (width-style width) style)]
    (cond-> {}
      class       (assoc :class class)
      (seq style) (assoc :style style))))

(defn- merge-cell-attrs
  "Merge extra cell attrs into the static ones, joining class strings and
   merging style maps instead of overwriting them."
  [attrs extra]
  (let [class (str/join " " (remove str/blank? [(:class attrs) (:class extra)]))
        style (merge (:style attrs) (:style extra))]
    (cond-> attrs
      (not (str/blank? class)) (assoc :class class)
      (seq style)              (assoc :style style))))

(defn- style-extra
  "Extra td attrs from a column's :style-fn; a legacy fn is applied directly."
  [col value]
  (let [s (:style-fn col)]
    (cond
      (fn? s)      (or (s value) {})
      (keyword? s) (or (rtable-fmt/style-rtable-col s value) {})
      :else        {})))

(defn- default-render-cell
  "Default cell content: resolve, format and wrap in an element (optionally
   tagged by a column :attrs fn)."
  [{:keys [attrs el-type] :as col} row]
  (let [value (col-value row col)
        attrs (if (fn? attrs) (attrs value) {})]
    [(or el-type :span) attrs (format-value col value)]))

(defn- cell-td [table-render-cell col row]
  (let [rc (or (:render-cell col) table-render-cell)
        value (col-value row col)]
    [:td (merge-cell-attrs (cell-attrs col) (style-extra col value))
     (rc col row)]))

(defn- render
  "Build the table hiccup from an opts map (columns/rows plus :class/:style
   table passthrough and an optional :row-height applied to body rows)."
  [{:keys [columns rows class style row-height render-cell] :as _opts}]
  (if (empty? rows)
    [:div {:style {:padding "1rem" :color "#888"}} "No rows in this table."]
    (let [rc (or render-cell default-render-cell)]
      (into [:table {:class (or class default-class) :style style}]
            (concat
             [(into [:thead]
                    [(into [:tr] (map (fn [c] [:th (cell-attrs c) (header-label c)]) columns))])]
             [(into [:tbody]
                    (for [row rows]
                      (into [:tr (cond-> {}
                                   row-height (assoc :style {:height row-height}))]
                            (map (fn [c] (cell-td rc c row)) columns))))])))))

(defrecord Rtable [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_]
    (render {:rows data :class default-class :columns (default-columns data)}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (if-let [spec (rtable-fmt/normalize-spec spec)]
      (render (cond-> {:rows data
                       :columns (:columns spec)
                       :row-height (:row-height spec)}
                window (assoc :style (assoc (dissoc (spec-plot/window->style window) :height)
                                            :width "100%")
                              :class (str "table-head-fixed " default-class))))
      [:div {:style {:padding "1rem" :color "#888"}}
       "Invalid rtable spec: expected {:row-height <length> :columns [<column> ...]}"]))
  (default-spec [_]
    {:row-height rtable-fmt/default-row-height
     :columns (or (default-columns data) [])})
  (spec-editor [v spec code-a]
    (rtable-fmt/rtable-format-edit-ui (:data v) {:spec spec :code-a code-a})))

(defn rtable
  [data]
  (->Rtable data))
