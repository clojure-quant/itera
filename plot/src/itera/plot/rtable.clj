(ns itera.plot.rtable
  "Server-rendered HTML table viewer (pure hiccup, no JS).

   The public `rtable` fn takes only the row `data`:
     (rtable [{...} ...])

   Column definitions (the editable spec) come from the host through
   itera.plot-ui.protocol and mirror the dali-table `rtable` viewer's
   server-expressible parts: column `:path` / `:header` / `:attrs` and basic
   `:class` / `:style` / `:max-width` styling.  Client-only knobs (sorting,
   virtualization) are out of scope.

   With no spec given, columns are derived from the first row's keys (the
   header via the existing `header-label`).  The spec must be plain data (no
   `:format`): the spec editor is an EDN textarea and fns cannot round-trip.

   The returned `Rtable` record implements both itera.value.protocol (renders
   the default table) and itera.plot-ui.protocol (`:columns` as the editable
   spec)."
  (:require [itera.input.codemirror :as input-cm]
            [itera.plot-ui.protocol :as spec-plot]
            [itera.value.protocol]))

(def head
  [[:link {:rel  "stylesheet"
           :href "/itera/plot/rtable/rtable.css"}]])

(defn- col-value
  "Resolve a row's value for a column via its :path (keyword/vector) or :expr."
  [row {:keys [path expr]}]
  (cond
    path (if (vector? path) (get-in row path) (get row path))
    expr (expr row)
    :else nil))

(defn- format-boolean [b]
  (case b true "\u2713" false "\u2715" ""))

(defn- default-format [data]
  (if (boolean? data) (format-boolean data) data))

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

(defn- cell-attrs [{:keys [class style max-width] :as _col}]
  (let [style (merge (max-width-style max-width) style)]
    (cond-> {}
      class       (assoc :class class)
      (seq style) (assoc :style style))))

(defn- default-render-cell
  "Default cell content: resolve, format and wrap in an element (optionally
   tagged by a column :attrs fn)."
  [{:keys [format attrs el-type] :or {format default-format attrs (fn [_] {})} :as col} row]
  [(or el-type :span) (attrs (col-value row col)) (format (col-value row col))])

(defn- cell-td [table-render-cell col row]
  (let [rc (or (:render-cell col) table-render-cell)]
    [:td (cell-attrs col) (rc col row)]))

(defn- render
  "Build the table hiccup from an opts map (columns/rows plus :class/:style
   table passthrough)."
  [{:keys [columns rows class style render-cell] :as _opts}]
  (if (empty? rows)
    [:div {:style {:padding "1rem" :color "#888"}} "No rows in this table."]
    (let [rc (or render-cell default-render-cell)]
      (into [:table {:class class :style style}]
            (concat
             [(into [:thead]
                    [(into [:tr] (map (fn [c] [:th (cell-attrs c) (header-label c)]) columns))])]
             [(into [:tbody]
                    (for [row rows]
                      (into [:tr] (map (fn [c] (cell-td rc c row)) columns))))])))))

(defrecord Rtable [data]
  itera.value.protocol/itera-convertable
  (to-hiccup [_]
    (render {:rows data :columns (default-columns data)}))
  spec-plot/itera-spec-plot
  (to-formatted-hiccup [_ spec window]
    (render (cond-> {:rows data
                     :columns (or spec (default-columns data))}
              window (assoc :style (spec-plot/window->style window)))))
  (default-spec [_] (default-columns data))
  (spec-editor [_ _spec code-a]
    (input-cm/codemirror-ui code-a {:style {:height "440px"}})))

(defn rtable
  [data]
  (->Rtable data))
