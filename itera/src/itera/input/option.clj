(ns itera.input.option
  "A table UI to author an `itera.option.core/options-ui` spec.

   Each row is one option map
   {:path [<kw>] :type <editor-kw> :name <string|nil> :spec <string>} held in
   the caller's atom.  Edits/add/delete are server-side hyper actions
   that write the atom (the caller passes a hyper cursor so writes re-render).

   `create-option-spec` compiles each row's string :spec into a value with
   itera.util.clj/compile-formula so it can be fed to options-ui.

   An optional `predefined` argument is a vector of
   {:name <string> :rows [<row> ...]} configurations.  When supplied, a
   \"Load predefined\" combobox renders above the table; selecting one resets
   the caller's atom with that preset's rows (each row normalized by
   `load-row`).  The combobox selection is tracked in
   [:itera-option-input :preset] alongside [:itera-option-input :new-editor]
   and [:itera-option-input :spec-open]; those paths are fixed, so this
   component is single-instance per tab.

   Must be called inside a Hyper render context."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [itera.option.core :as options]
            [itera.util.clj :as util]))

(defn editor-types
  "All selectable option editor :type keywords (every `editor` method
   except the :default fallback), sorted by name."
  []
  (->> (keys (methods options/editor))
       (remove #{:default})
       (sort-by name)))

(defn- sanitize-name
  "Name text with all whitespace removed (the name textbox must not contain
   spaces; this is the defensive server-side pass)."
  [s]
  (str/replace (or s "") #"\s" ""))

(defn- new-row [type] {:path [] :type type :name nil :spec ""})

(defn- load-row
  "Normalize a predefined row: :path as a vector, :type defaulted, :name
   falling back to the path keyword's name, :spec as a string."
  [{:keys [path type spec] row-name :name}]
  {:path (vec path)
   :type (or type :string)
   :name (or row-name (some-> path first name))
   :spec (or spec "")})

(defn- delete-row
  "Vector with the row at idx removed."
  [rows idx]
  (vec (keep-indexed (fn [i r] (when (not= i idx) r)) rows)))

(def ^:private delete-col-width "1.6rem")
(def ^:private name-col-width   "5rem")
(def ^:private editor-col-width "6.5rem")
(def ^:private spec-col-width   "2rem")

(def ^:private delete-btn-style
  {:border "1px solid #fca5a5"
   :background "#fef2f2"
   :color "#b91c1c"
   :cursor "pointer"
   :border-radius "0.375rem"
   :font-size "0.9rem"
   :line-height "1"
   :padding "0.2rem 0.45rem"})

(def ^:private add-btn-style
  {:border "1px solid #2563eb" :background "#2563eb" :color "white"
   :cursor "pointer" :border-radius "0.375rem" :font-weight "700"
   :font-size "1rem" :line-height "1" :padding "0.3rem 0.6rem"})

(def ^:private spec-btn-style
  {:border "1px solid #d1d5db" :background "white" :color "#374151"
   :cursor "pointer" :border-radius "0.375rem" :font-size "0.9rem"
   :line-height "1" :padding "0.25rem 0.6rem"})

(def ^:private preset-bar-style
  {:display "flex" :align-items "center" :gap "0.5rem"
   :padding "0.5rem 0.6rem" :margin-bottom "0.5rem"
   :background "#f9fafb" :border "1px solid #e5e7eb"
   :border-radius "0.375rem"})

(def ^:private preset-label-style
  {:font-size "0.78rem" :font-weight "600" :text-transform "uppercase"
   :letter-spacing "0.03em" :color "#374151"})

(def ^:private preset-select-style
  {:padding "0.3rem 0.5rem" :border "1px solid #d1d5db"
   :border-radius "0.375rem" :font-size "0.9rem" :background "white"})

(def ^:private table-style
  {:border-collapse "collapse" :table-layout "fixed"
   :font-size "0.8rem" :font-family "system-ui, sans-serif"})

(def ^:private th-style
  {:text-align "left" :padding "0.3rem 0.4rem" :border-bottom "2px solid #e5e7eb"
   :background "#f9fafb" :color "#374151" :font-weight "600"
   :font-size "0.7rem" :text-transform "uppercase" :letter-spacing "0.03em"
   :box-sizing "border-box"})

(def ^:private td-style
  {:padding "0.3rem 0.4rem" :border-bottom "1px solid #eef2f7"
   :vertical-align "middle" :box-sizing "border-box"})

(def ^:private name-input-style
  {:width "100%" :box-sizing "border-box" :padding "0.25rem 0.4rem"
   :border "1px solid #d1d5db" :border-radius "0.375rem" :font-size "0.8rem"})

(def ^:private editor-badge-style
  {:display "inline-block" :padding "0.1rem 0.4rem" :background "#eef2ff"
   :color "#3730a3" :border-radius "999px" :font-size "0.7rem"
   :font-weight "600" :max-width "100%" :box-sizing "border-box"
   :white-space "nowrap" :overflow "hidden" :text-overflow "ellipsis"
   :vertical-align "middle"})

(def ^:private modal-backdrop-style
  {:position "fixed" :inset "0" :background "rgba(17,24,39,0.45)"
   :display "flex" :align-items "center" :justify-content "center"
   :z-index "1000" :padding "1rem"})

(def ^:private modal-card-style
  {:background "white" :border-radius "0.5rem" :padding "1rem"
   :box-shadow "0 10px 30px rgba(0,0,0,0.2)" :width "min(560px, 100%)"
   :display "flex" :flex-direction "column" :gap "0.6rem"})

(def ^:private modal-title-style {:font-weight "600" :color "#374151"})

(def ^:private modal-textarea-style
  {:width "100%" :box-sizing "border-box" :font-family "monospace"
   :font-size "0.85rem" :padding "0.5rem" :border "1px solid #d1d5db"
   :border-radius "0.375rem" :resize "vertical"})

(def ^:private modal-actions-style
  {:display "flex" :justify-content "flex-end" :gap "0.5rem"})

(defn option-input
  "Render the option-spec table bound to options-a (a hyper cursor/atom whose
   value is the vector of rows).  Add/delete/edit write options-a.  The editor
   for a new row is chosen in the footer combobox ([:itera-option-input
   :new-editor] tab cursor) and the spec popup's open row is tracked in
   [:itera-option-input :spec-open]; when a `predefined` vector of
   {:name <string> :rows [<row> ...]} is supplied, a \"Load predefined\"
   combobox tracks its selection in [:itera-option-input :preset].  All three
   paths are fixed, so this component is single-instance per tab."
  ([options-a] (option-input options-a nil))
  ([options-a predefined]
   (let [rows           @options-a
         editors        (editor-types)
         default-editor (or (some #(when (= % :string) %) editors)
                            (first editors)
                            :string)
         editor-a       (h/tab-cursor [:itera-option-input :new-editor]
                                      default-editor)
         open-a         (h/tab-cursor [:itera-option-input :spec-open] nil)
         preset-a       (h/tab-cursor [:itera-option-input :preset] "")]
     [:div.option-input
      (when (seq predefined)
        [:div {:style preset-bar-style}
         [:label {:style preset-label-style} "Load predefined"]
         [:select {:style preset-select-style
                   :data-on:change
                   (h/expr
                    (h/action
                     (when-let [p (some #(when (= (:name %) $value) %) predefined)]
                       (reset! options-a (mapv load-row (:rows p)))
                       (reset! open-a nil))
                     (reset! preset-a ""))
                    (set! (.-value evt.target) ""))}
          [:option {:value "" :selected (= "" @preset-a)} "—"]
          (for [p predefined]
            [:option {:value (:name p)
                      :selected (= (:name p) @preset-a)}
             (:name p)])]])
      [:table {:style table-style}
      [:thead
       [:tr
        [:th {:style (assoc th-style :width delete-col-width)} ""]
        [:th {:style (assoc th-style :width name-col-width)} "Name"]
        [:th {:style (assoc th-style :width editor-col-width)} "Editor"]
        [:th {:style (assoc th-style :width spec-col-width)} "Spec"]]]
      [:tbody
       (for [[idx row] (map-indexed vector rows)]
         [:tr {:key idx}
          ;; delete
          [:td {:style (assoc td-style :text-align "center")}
           [:button {:type "button" :title "Delete" :aria-label "Delete"
                     :style delete-btn-style
                     :data-on:click (h/action (swap! options-a delete-row idx)
                                              (reset! open-a nil))}
            "×"]]
          ;; name -> :path [(keyword text)] and :name text
          [:td
           [:input {:type "text"
                    :value (or (:name row) (some-> row :path first name) "")
                    :style name-input-style
                    :data-on:keydown
                    (h/expr (when (= evt.key " ") (.preventDefault evt)))
                    :data-on:change
                    (h/action
                     (swap! options-a update idx
                            (fn [r]
                              (let [t (sanitize-name $value)]
                                (assoc r
                                       :path (if (seq t) [(keyword t)] [])
                                       :name (when (seq t) t))))))}]]
          ;; editor type (badge)
          [:td [:span {:style editor-badge-style :title (name (:type row))}
                (name (:type row))]]
          ;; spec -> "..." button opens the popup for this row
          [:td
           [:button {:type "button" :title "Edit spec" :aria-label "Edit spec"
                     :style spec-btn-style
                     :data-on:click (h/action (reset! open-a idx))}
            "..."]
           (when (= idx @open-a)
             [:div {:style modal-backdrop-style
                    :data-on:click (h/action (reset! open-a nil))}
              [:div {:style modal-card-style
                     :data-on:click (h/expr (.stopPropagation evt))}
               [:div {:style modal-title-style} "spec"]
               [:textarea {:rows 8 :style modal-textarea-style
                           :data-on:change
                           (h/action (swap! options-a update idx assoc :spec $value))}
                (or (:spec row) "")]
               [:div {:style modal-actions-style}
                [:button {:type "button" :style add-btn-style
                          :data-on:click (h/action (reset! open-a nil))}
                 "Close"]]]])]])]
      [:tfoot
       [:tr
        [:td {:style (assoc td-style :text-align "center")}
         [:button {:type "button" :title "Add option" :aria-label "Add option"
                   :style add-btn-style
                   :data-on:click (h/action (swap! options-a conj (new-row @editor-a)))}
          "+"]]
        [:td {:style td-style}]
        [:td {:style td-style}
         [:select {:style {:width "100%" :min-width 0 :font-size "0.8rem"}
                   :data-on:change
                   (h/action (reset! editor-a (keyword $value)))}
          (for [e editors]
            [:option {:value (name e) :selected (= e @editor-a)} (name e)])]]
        [:td {:style td-style}]]]]])))

(defn create-option-spec
  "Compile each row whose :spec is a non-blank string in the workbook namespace
   via itera.util.clj/compile-formula, and return the vector of option maps ready
   for itera.option.core/options-ui.  A blank :spec becomes nil (it has no value
   form to evaluate); rows whose :spec is not a string are passed through
   unchanged."
  [workbook options]
  (mapv (fn [{:keys [spec] :as opt}]
          (if (string? spec)
            (assoc opt :spec (when-not (str/blank? spec)
                               (:value (util/compile-formula workbook spec))))
            opt))
        options))
