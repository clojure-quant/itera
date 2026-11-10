(ns itera.options
  "Server-side options UI from a spec (pink-gorilla/options spec format).
   Renders hiccup and writes edits server-side into an options cursor through
   hyper actions.  All functions must be called inside a Hyper render context."
  (:require [hyper.core :as h]))

(defn norm-path
  "Normalize an option :path (keyword or vector) to a vector."
  [path]
  (cond
    (keyword? path) [path]
    (vector? path)  (vec path)
    :else           [path]))

(defn resolve-spec
  "Resolve an algo's :.../options value to a spec vector.
   fn -> (f ctx); sequential -> as-is; anything else (nil, {}) -> []."
  [x ctx]
  (cond
    (fn? x)         (or (x ctx) [])
    (sequential? x) (vec x)
    :else           []))

;; --- select spec normalization (mirrors pink-gorilla/options) ---------------

(defn- vals->spec [vals]
  (map-indexed (fn [idx v] {:id (str (inc idx)) :name (str v) :val v}) vals))

(defn- spec->spec [specs]
  (map (fn [{:keys [id] :as o}] (assoc o :val id :id (str id))) specs))

(defn- normalize-select [spec]
  (if (and (seq spec) (map? (first spec)))
    (spec->spec spec)
    (vals->spec (or spec []))))

(defn- id->val [norm-spec] (into {} (map (juxt :id :val) norm-spec)))

(defn- current-id [norm-spec current]
  (some (fn [{:keys [id val]}] (when (= val current) id)) norm-spec))

(defn- current-ids [norm-spec current]
  (let [sel (set current)]
    (into [] (comp (filter #(contains? sel (:val %))) (map :id)) norm-spec)))

;; --- editors ----------------------------------------------------------------

(defn- editor [opt value set-fn group]
  (let [{:keys [type name class size]} opt
        style (or (:style opt) {})
        path  (norm-path (:path opt))]
    (case type
      :bool
      [:input {:class class :style style :type "checkbox"
               :checked (boolean value)
               :data-on:change (h/action (set-fn path $checked))}]

      :string
      [:input {:class class :style style :type "text"
               :value (str (or value ""))
               :placeholder (or name "")
               :data-on:change (h/action (set-fn path $value))}]

      :select
      (let [ns-spec (normalize-select (:spec opt))
            d       (id->val ns-spec)
            sel     (current-id ns-spec value)]
        (into [:select {:class class :style style
                        :data-on:change (h/action (set-fn path (get d $value)))}]
              (for [{id :id nm :name} ns-spec]
                [:option {:value id :selected (= id sel)} nm])))

      :select-multiple
      (let [ns-spec (normalize-select (:spec opt))
            d       (id->val ns-spec)
            ids     (current-ids ns-spec (or value []))
            sig     (h/signal (into [:itera-options group] path) ids)]
        (into [:select {:class class :style style
                        :multiple true :size (or size 5)
                        :data-bind sig
                        :data-on:change (h/action (set-fn path (mapv d @sig)))}]
              (for [{id :id nm :name} ns-spec]
                (let [i (str id)]
                  [:option {:value i :selected (boolean (some #{i} ids))} nm]))))

      :view
      [:span {:class class :style style} (pr-str value)]

      [:span {:class class :style style} (pr-str value)])))

;; --- public entry -----------------------------------------------------------

(defn options-ui
  "Render an options UI.
   opts:
     :spec    vector of option maps (see pink-gorilla/options format)
     :value   current options map (deref of the options cursor)
     :set-fn  (fn [path value]) invoked inside the server action to write the
              cursor, e.g. (fn [p v] (swap! options-a assoc-in p v))
     :group   keyword used to disambiguate signal names (:calculate / :plot)
     :class :style  container passthrough"
  [{:keys [spec value set-fn group class style]}]
  (into [:div.options-ui {:class class :style style}]
        (map (fn [opt]
               (if (= :label (:type opt))
                 [:div.options-group-header {:class (:class opt)
                                             :style (:style opt)}
                  (or (:name opt) "")]
                 (let [path (norm-path (:path opt))]
                   [:div.options-row
                    (when (:name opt) [:span.options-label (:name opt)])
                    (editor opt (get-in value path) set-fn group)]))))
        spec))
