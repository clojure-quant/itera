(ns itera.plot.rtable
  "Server-rendered HTML table viewer (pure hiccup, no JS).

   Mirrors the dali-table `rtable` viewer for the server-expressible parts:
   column `:path` / `:header` / `:format` / `:render-cell` / `:attrs` and
   basic `:class` / `:style` / `:max-width` styling.  Client-only knobs
   (sorting, virtualization) are out of scope.

   opts:
     :columns  [{:path :close :header \"Close\" :format (fn [v] ...)} ...]
     :rows     [{:close 10.4} ...]
     :render-cell  table-level cell fn (fn [col row]) -> content (hiccup/value)
     :class :style  table passthrough")

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

(defn rtable
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
