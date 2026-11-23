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

   `data` may be any table value implementing
   itera.value.protocol/itera-table-info (a vector/seq of row maps or a
   tech.ml/tablecloth dataset); it is stored as-is and every count / slice /
   type / row-map step goes through that protocol.  With no spec given,
   columns are derived from the table's column-info keys (the header via the
   existing `header-label`).  The spec editor is a table UI
   (itera.plot.rtable-format-ui/rtable-format-edit-ui) that writes plain EDN,
   so custom format/style methods must be registered by keyword.

   The returned `Rtable` record implements both itera.value.protocol (renders
   the default table) and itera.value.protocol (`:columns` as the editable
   spec).

   It also implements itera-page-plot: the interact-tab cell UI renders one
   page at a time and shows a page editor below the table with first/previous/
   next/last buttons and an editable page number.  Rows per page come from the
   window height, the fixed header height and the spec's :row-height (not a
   constant).  With no positive window height the table is not paginated, and
   the page slice is taken through `select-rows`.  All page controls are always
   rendered and are disabled at the ends (or when there is only one page).
   `to-hiccup` still renders the full table."
  (:require [clojure.string :as str]
            [itera.plot.rtable-default-column-formatter :as rtable-default-fmt]
            [itera.plot.rtable-format-ui :as rtable-fmt]
            [itera.plot.rtable-page-editor :as page-editor]
            ;; Loading these itera value namespaces (from the itera project)
            ;; registers itera-table-info for vectors, seqs and
            ;; tech.ml/tablecloth datasets; the record accesses :data only
            ;; through that protocol.
            [itera.value.dataset-table-info]
            [itera.value.protocol :as spec-plot]
            [itera.value.vector-table-info]))

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

(defn- table-column-info
  "Column name -> inferred type for DATA; {} when DATA is nil or does not
   implement itera-table-info."
  [data]
  (if (and data (satisfies? spec-plot/itera-table-info data))
    (spec-plot/column-info data)
    {}))

(defn- table-rows
  "Rows of DATA as a sequence of maps; [] when DATA is nil or does not
   implement itera-table-info."
  [data]
  (if (and data (satisfies? spec-plot/itera-table-info data))
    (spec-plot/as-seq-of-maps data)
    []))

(defn- default-columns
  "Columns derived from the table's column-info keys, in order; the header
   comes from header-label.  [] when there are no columns."
  [data]
  (mapv (fn [k] {:path k}) (keys (table-column-info data))))

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

(def header-height
  "Pixels reserved for the sticky table header when computing rows per page."
  32)

(def ^:private px-per-unit
  {"px" 1.0, "rem" 16.0, "em" 16.0})

(defn css-length->px
  "Parse a CSS length (px/rem/em, unitless = px) to px.  rem/em use a 16px
   base.  nil when S is not a length."
  [s]
  (when-let [[_ n unit] (re-matches #"(?i)\s*(\d+(?:\.\d+)?)\s*(px|rem|em)?\s*"
                                    (str s))]
    (* (parse-double n) (px-per-unit (some-> unit str/lower-case) 1.0))))

(defn row-height-px
  "Pixel body row height for SPEC, falling back to default-row-height when
   :row-height is missing or not a CSS length."
  [spec]
  (or (some-> (:row-height (rtable-fmt/normalize-spec spec)) css-length->px)
      (css-length->px rtable-fmt/default-row-height)))

(defn rows-per-page
  "Rows that fit in WINDOW: floor((height - header-height) / row-height), at
   least 1.  nil when WINDOW has no positive :height (do not paginate)."
  [spec window]
  (let [height (:height window)
        row-px (row-height-px spec)]
    (when (and (number? height) (pos? height) row-px (pos? row-px))
      (max 1 (long (Math/floor (/ (- height header-height) (double row-px))))))))

(defn page-count
  "Pages needed for ROW-COUNT at ROW-SIZE; 1 when ROW-SIZE is nil."
  [row-count row-size]
  (if (and row-size (pos? row-size))
    (max 1 (quot (+ row-count (dec row-size)) row-size))
    1))

(defn- page-geometry [row-count spec window]
  (let [row-size (rows-per-page spec window)]
    {:row-size row-size
     :total    (page-count row-count row-size)}))

(defn- clamp-current [total current]
  (max 1 (min (or current 1) total)))

(defrecord Rtable [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_]
    (render {:rows (table-rows data)
             :class default-class
             :columns (default-columns data)}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (if-let [spec (rtable-fmt/normalize-spec spec)]
      (render (cond-> {:rows (table-rows data)
                       :columns (:columns spec)
                       :row-height (:row-height spec)}
                window (assoc :style (assoc (dissoc (spec-plot/window->style window) :height)
                                            :width "100%")
                              :class (str "table-head-fixed " default-class))))
      [:div {:style {:padding "1rem" :color "#888"}}
       "Invalid rtable spec: expected {:row-height <length> :columns [<column> ...]}"]))
  (default-spec [_]
    {:row-height rtable-fmt/default-row-height
     :columns (default-columns data)})
  (spec-editor [_ spec code-a]
    (rtable-fmt/rtable-format-edit-ui data {:spec spec :code-a code-a}))
  spec-plot/itera-page-plot
  (to-formatted-paged-hiccup [v spec window page]
    (let [n (spec-plot/total-rows data)
          {:keys [row-size total]} (page-geometry n spec window)
          current (clamp-current total (:page/current page 1))
          sliced (if (and row-size (pos? row-size))
                   (spec-plot/select-rows data (* (dec current) row-size)
                                          (* current row-size))
                   data)]
      (spec-plot/to-formatted-hiccup (assoc v :data sliced) spec window)))

  (init-page [_ spec window]
    (let [{:keys [row-size total]} (page-geometry (spec-plot/total-rows data)
                                                  spec window)]
      {:page/row-size row-size
       :page/total    total
       :page/current  1}))

  (page-editor [_ spec window page-a]
    (let [{:keys [total]} (page-geometry (spec-plot/total-rows data) spec window)]
      (page-editor/page-editor-view total page-a))))

(defn rtable
  [data]
  (->Rtable (or data [])))
