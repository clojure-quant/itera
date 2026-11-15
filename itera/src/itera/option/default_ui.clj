(ns itera.option.default-ui
  "The built-in option editors.

   Registers the generic `itera.options/editor` methods dispatched on the
   built-in option :type values: :bool :string :date :select :select-multiple
   :view :default.  A :date value is a java.time.LocalDate (native
   <input type=\"date\">; nil when cleared).

   This namespace is not required by itera.options (that would be circular);
   it is loaded from the itera project's `itera-plots.edn` :option-ui entry by
   `itera.hyper/start`."
  (:require [hyper.core :as h]
            [itera.options :as options]))

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
   :path  (options/norm-path (:path opt))})

(defmethod options/editor :bool [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)]
    [:input {:class class :style style :type "checkbox"
             :checked (boolean value)
             :data-on:change (h/action (set-fn path $checked))}]))

(defmethod options/editor :string [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)
        name (:name opt)]
    [:input {:class class :style style :type "text"
             :value (str (or value ""))
             :placeholder (or name "")
             :data-on:change (h/action (set-fn path $value))}]))

(defmethod options/editor :date [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)]
    [:input {:class class :style style :type "date"
             :value (date->str value)
             :data-on:change (h/action (set-fn path (str->date $value)))}]))

(defmethod options/editor :select [opt value set-fn _update-fn _group]
  (let [{:keys [class style path]} (base-attrs opt)
        ns-spec (normalize-select (:spec opt))
        d       (id->val ns-spec)
        sel     (current-id ns-spec value)]
    (into [:select {:class class :style style
                    :data-on:change (h/action (set-fn path (get d $value)))}]
          (for [{id :id nm :name} ns-spec]
            [:option {:value id :selected (= id sel)} nm]))))

(defmethod options/editor :select-multiple [opt value set-fn _update-fn group]
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

(defmethod options/editor :view [opt value _set-fn _update-fn _group]
  (let [{:keys [class style]} (base-attrs opt)]
    [:span {:class class :style style} (pr-str value)]))

(defmethod options/editor :default [opt value _set-fn _update-fn _group]
  (let [{:keys [class style]} (base-attrs opt)]
    [:span {:class class :style style} (pr-str value)]))