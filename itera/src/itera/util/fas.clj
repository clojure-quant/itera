(ns itera.util.fas
  "Server-rendered Font Awesome solid icons.

   `fas` renders an icon and transparently arranges for the Font Awesome
   stylesheet to be present in the tab's document head.  The stylesheet is
   loaded at most once per browser tab: the first `fas` call sets the tab
   cursor `[:itera.util.fas :css-loaded?]`, and `head` emits the `<link>` only
   while that flag is true.

   `head` must be registered as a dynamic Hyper head function (see
   itera.plot-discover and itera.hyper/start).  It reads through
   `hyper.core/tab-cursor`, not the raw application state, because Hyper
   serializes the body before resolving the head but has not yet flushed the
   render overlay.  The cursor sees the overlay write made by `fas` in the same
   render; a direct app-state read would not.

   The link is never emitted from `fas` itself: Hyper morphs the body on every
   SSE re-render, which would remove a body-level `<link>`.

   Both `fas` and `head` rely on a Hyper render/action context; they are not
   plain hiccup helpers outside a Hyper request."
  (:require [clojure.string :as str]
            [hyper.context :as context]
            [hyper.core :as h]))

(def css-href
  "Font Awesome 6.5.1 stylesheet URL.  Keep this pin in sync with the
   generated `fontawesome-solid.edn` snapshot used by the demo."
  "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.1/css/all.min.css")

(def ^:private cursor-path
  [:itera.util.fas :css-loaded?])

(defn icon-class
  "Normalize :fa-user-nurse, :user-nurse, \"fa-user-nurse\", or
   \"user-nurse\" to \"fa-solid fa-user-nurse\"."
  [icon]
  (let [nm (cond
             (keyword? icon) (name icon)
             (symbol? icon)  (name icon)
             (string? icon)  icon
             :else           (str icon))
        nm (if (str/starts-with? nm "fa-") nm (str "fa-" nm))]
    (str "fa-solid " nm)))

(defn- mark-css-loaded!
  "Set the per-tab CSS flag on first use.

   This is a deliberate render-time cache update, not application state.  The
   Hyper render purity guard treats cursor writes during a render as effects,
   so the single `reset!` runs with the guard unbound instead of logging a
   warning on the first icon use.  Only this write is exempted; the guard stays
   active for the rest of the render."
  []
  (let [loaded (h/tab-cursor cursor-path false)]
    (when-not @loaded
      (binding [context/*render-guard* nil]
        (reset! loaded true)))))

(defn fas
  "Render a Font Awesome solid icon, e.g. `(fas :fa-user-nurse)`, and mark the
   Font Awesome stylesheet for inclusion in this tab's head.

   Options:
     :label - accessible label; adds role=\"img\" and aria-label instead of
              aria-hidden
     :class - extra class name(s) appended to `fa-solid fa-...`; a string or
              a sequential of strings
     :style - inline style map
     :title - tooltip text
     :id    - element id"
  ([icon] (fas icon nil))
  ([icon opts]
   (mark-css-loaded!)
   (let [{:keys [label style title id class]} opts
         extra (cond
                 (string? class)     class
                 (sequential? class) (str/join " " class)
                 :else               nil)
         attrs (cond-> {:class (cond-> (icon-class icon)
                                 (seq extra) (str " " extra))}
                 label         (assoc :role "img" :aria-label label)
                 (not label)   (assoc :aria-hidden "true")
                 style         (assoc :style style)
                 title         (assoc :title title)
                 id            (assoc :id id))]
     [:i attrs])))

(defn head
  "Hyper dynamic head function: include the Font Awesome stylesheet once the
   current tab has used `fas`.

   Returns nil when the tab has never rendered an icon, so pages that do not
   use `fas` pay nothing.  Once the flag is set it stays set for the tab, which
   also keeps the link present when navigating to a page without icons."
  [_req]
  (let [loaded (h/tab-cursor cursor-path)]
    (when @loaded
      [[:link {:rel "stylesheet" :href css-href}]])))