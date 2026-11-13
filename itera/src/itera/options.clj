(ns itera.options
  "Server-side options UI from a spec (pink-gorilla/options spec format).
   Renders hiccup and writes edits server-side into an options cursor through
   hyper actions.  All functions must be called inside a Hyper render context.

   Built-in option :type values: :bool :string :date :select :select-multiple
   :view :label.  A :date value is a java.time.LocalDate (native
   <input type=\"date\">; nil when cleared).  Editors are plain functions of
   the `editor` multimethod, dispatched on (:type opt), so viewer projects add
   their own with (defmethod editor :type ...)."
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

(defn- date->str
  "Render a value as an <input type=\"date\"> value string (yyyy-MM-dd)."
  [v]
  (cond
    (instance? java.time.LocalDate v) (str v)
    (string? v)                       v
    :else                             ""))

(defn- str->date
  "Parse a yyyy-MM-dd string into a java.time.LocalDate (nil when blank/invalid)."
  [s]
  (when (seq s)
    (try (java.time.LocalDate/parse s) (catch Exception _ nil))))

(defn- base-attrs
  "Common render attributes for an option's editor: class, style, path."
  [opt]
  {:class (:class opt)
   :style (or (:style opt) {})
   :path  (norm-path (:path opt))})

;; The extension point: viewer projects add editors with
;;   (defmethod editor :my-type [opt value set-fn update-fn group] ...)
;; Dispatches on (:type opt).
(defmulti editor
  "Render an option editor.  Dispatches on (:type opt).
   Signature: (editor opt value set-fn update-fn group)."
  (fn [opt _value _set-fn _update-fn _group] (:type opt)))

(defmethod editor :bool [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)]
    [:input {:class class :style style :type "checkbox"
             :checked (boolean value)
             :data-on:change (h/action (set-fn path $checked))}]))

(defmethod editor :string [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)
        name (:name opt)]
    [:input {:class class :style style :type "text"
             :value (str (or value ""))
             :placeholder (or name "")
             :data-on:change (h/action (set-fn path $value))}]))

(defmethod editor :date [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)]
    [:input {:class class :style style :type "date"
             :value (date->str value)
             :data-on:change (h/action (set-fn path (str->date $value)))}]))

(defmethod editor :select [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)
        ns-spec (normalize-select (:spec opt))
        d       (id->val ns-spec)
        sel     (current-id ns-spec value)]
    (into [:select {:class class :style style
                    :data-on:change (h/action (set-fn path (get d $value)))}]
          (for [{id :id nm :name} ns-spec]
            [:option {:value id :selected (= id sel)} nm]))))

(defmethod editor :select-multiple [opt value set-fn _update-fn group]
  (let [{:keys [class style path]} (base-attrs opt)
        size    (:size opt)
        ns-spec (normalize-select (:spec opt))
        d       (id->val ns-spec)
        ids     (current-ids ns-spec (or value []))
        sig     (h/signal (into [:itera-options group] path) ids)]
    (into [:select {:class class :style style
                    :multiple true :size (or size 5)
                    :data-bind sig
                    :data-on:change (h/action (set-fn path (mapv d @sig)))}]
          (for [{id :id nm :name} ns-spec]
            (let [i (str id)]
              [:option {:value i :selected (boolean (some #{i} ids))} nm])))))

(defmethod editor :view [opt value _set-fn _update-fn _group]
  (let [{:keys [class style]} (base-attrs opt)]
    [:span {:class class :style style} (pr-str value)]))

(defmethod editor :default [opt value _set-fn _update-fn _group]
  (let [{:keys [class style]} (base-attrs opt)]
    [:span {:class class :style style} (pr-str value)]))

;; --- public entry -----------------------------------------------------------

(defn options-ui
  "Render an options UI.
   opts:
     :spec    vector of option maps (see pink-gorilla/options format)
     :value   current options map (deref of the options cursor)
     :set-fn  (fn [path value]) invoked inside the server action to write the
              cursor, e.g. (fn [p v] (swap! options-a assoc-in p v))
     :update-fn (fn [path f]) invoked inside the server action to read-modify-write
              the cursor value at path, e.g.
              (fn [p f] (swap! options-a update-in p f))
     :group   keyword used to disambiguate signal names (:calculate / :plot)
     :class :style  container passthrough

   Each option is rendered by the `editor` multimethod, dispatched on
   (:type opt); viewer projects extend it with (defmethod editor :type ...).
   A :label option is layout (a group header), not an editor."
  [{:keys [spec value set-fn update-fn group class style]}]
  (into [:div.options-ui {:class class :style style}]
        (map (fn [opt]
               (if (= :label (:type opt))
                 [:div.options-group-header {:class (:class opt)
                                             :style (:style opt)}
                  (or (:name opt) "")]
                 (let [path (norm-path (:path opt))]
                   [:div.options-row
                    (when (:name opt) [:span.options-label (:name opt)])
                    (editor opt (get-in value path) set-fn update-fn group)]))))
        spec))
