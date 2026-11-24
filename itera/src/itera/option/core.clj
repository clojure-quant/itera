(ns itera.option.core
  "Server-side options UI from a spec (pink-gorilla/options spec format).
   Renders hiccup and writes edits server-side into an options cursor through
   hyper actions.  All functions must be called inside a Hyper render context.

   The built-in editors dispatch on these :type values and live in
   `itera.option.default-ui`: :bool :string :int :double :date :select
   :select-multiple :view :default.  A :date value is a java.time.LocalDate
   (native <input type=\"date\">; nil when cleared); an :int/:double value is
   parsed with Integer/parseInt / Double/parseDouble (nil when blank/invalid).
   A :label option is layout, not an editor, and its styling comes from the
   stylesheet (no per-row :style).  The :container window editor lives in
   `itera.option.container`.

   Editors are plain functions of the `editor` multimethod, dispatched on
   (:type opt), so viewer projects add their own with
   (defmethod editor :type ...) in a namespace listed under :option-ui in their
   `itera-plots.edn`; `itera.hyper/start` loads those namespaces."
  (:require [clojure.string :as str]))

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

;; The extension point: viewer projects add editors with
;;   (defmethod editor :my-type [opt value set-fn update-fn group] ...)
;; Dispatches on (:type opt).
(defmulti editor
  "Render an option editor.  Dispatches on (:type opt).
   Signature: (editor opt value set-fn update-fn group)."
  (fn [opt _value _set-fn _update-fn _group] (:type opt)))

;; --- public entry -----------------------------------------------------------

(def head
  "Static head assets for the options UI (the minimal theme stylesheet)."
  [[:link {:rel "stylesheet" :href "/itera/options-ui.css"}]])

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
     :group   keyword used to disambiguate signal names (:calculate / :plot).
              Datastar drops keyword namespaces from signal names, so pass an
              unqualified keyword when a signal-backed editor (:select-multiple)
              is used; namespaced groups are normalized to their name for
              signal paths.
     :class :style  container passthrough

   Each option is rendered by the `editor` multimethod, dispatched on
   (:type opt); viewer projects extend it with (defmethod editor :type ...).
   The built-in editors are registered by `itera.option.default-ui`.  A :label
   option is layout (a group header), not an editor; its styling comes from the
   stylesheet, so its per-row :style is ignored."
  [{:keys [spec value set-fn update-fn group class style]}]
  (into [:div.options-ui
         {:class (str/join " " (remove str/blank? [class "options-theme-minimal"]))
          :style style}]
        (map (fn [opt]
               (if (= :label (:type opt))
                 [:div.options-group-header (or (:name opt) "")]
                 (let [path (norm-path (:path opt))]
                   [:div.options-row
                    [:span.options-label (or (:name opt) "")]
                    (editor opt (get-in value path) set-fn update-fn group)]))))
        spec))