(ns itera.plot.rtable-page-editor
  "Page navigation view for the rtable page editor.

   `page-editor-view` always renders first/previous/next/last page buttons plus
   an editable page-number box.  Controls that do not apply are rendered
   disabled instead of hidden.  It builds the Hyper actions from the page cursor
   itself, so this namespace depends on Hyper.  With no page cursor the buttons
   and the box are disabled and no actions are registered."
  (:require [hyper.core :as h]))

(defn- clamp-current [total current]
  (max 1 (min (or current 1) total)))

(def ^:private button-style
  {:font-size "0.75rem" :padding "0.15rem 0.45rem"
   :border "1px solid #d1d5db" :border-radius "0.25rem"
   :background "#f3f4f6" :color "#111827" :cursor "pointer"})

(def ^:private disabled-button-style
  {:font-size "0.75rem" :padding "0.15rem 0.45rem"
   :border "1px solid #e5e7eb" :border-radius "0.25rem"
   :background "#f9fafb" :color "#9ca3af" :cursor "not-allowed"})

(defn page-editor-view
  "Render page navigation for TOTAL pages and page cursor PAGE-A.

   TOTAL is the render-time page count computed from the data, spec and window.
   When PAGE-A is present, it is deref'd for the current position and is mutated
   by the first/previous/next/last and jump actions.  All four buttons and the
   page box are always rendered; controls that do not apply (or a missing page
   cursor) are disabled.  With PAGE-A nil no actions are registered."
  [total page-a]
  (let [total   (or total 1)
        current (clamp-current total (when page-a (:page/current @page-a)))
        first-a  (when page-a
                   (h/action {:as "first-page"}
                     (swap! page-a assoc :page/current 1)))
        prev-a   (when page-a
                   (h/action {:as "previous-page"}
                     (swap! page-a update :page/current
                            #(max 1 (dec (clamp-current total %))))))
        next-a   (when page-a
                   (h/action {:as "next-page"}
                     (swap! page-a update :page/current
                            #(min total (inc (clamp-current total %))))))
        last-a   (when page-a
                   (h/action {:as "last-page"}
                     (swap! page-a assoc :page/current total)))
        jump-a   (when page-a
                   (h/action {:as "jump-to-page"
                              :when "evt.key === 'Enter'"}
                     (when-let [n (parse-long (str $value))]
                       (swap! page-a assoc :page/current
                              (clamp-current total n)))))
        reset-a  (when page-a
                   (h/expr (set! (.-value evt.target) ~(str current))))
        input?   (and page-a (> total 1))
        button   (fn [attr label title enabled?]
                   [:button (cond-> {:type "button"
                                     :title title
                                     :aria-label title
                                     :style (if enabled?
                                              button-style
                                              disabled-button-style)}
                              enabled?       (assoc :data-on:click attr)
                              (not enabled?) (assoc :disabled true))
                    label])
        input-attrs (cond-> {:type "text"
                             :inputmode "numeric"
                             :pattern "[0-9]*"
                             :aria-label "Page"
                             :title "Page number; press Enter to jump"
                             :value (str current)
                             :style {:width (str (max 3 (count (str total))) "ch")
                                     :padding "0.05rem 0.2rem"
                                     :font-family "monospace"
                                     :font-size "inherit"
                                     :text-align "right"}}
                      (not input?) (assoc :disabled true)
                      input?       (assoc :data-on:keydown jump-a
                                          :data-on:blur reset-a))]
    [:div {:style {:display "flex"
                   :align-items "center"
                   :justify-content "flex-end"
                   :gap "0.5rem"
                   :padding "0.35rem 0"
                   :font-size "0.8rem"
                   :font-family "monospace"}}
     (button first-a "|<" "First page" (and page-a (> current 1)))
     (button prev-a "<" "Previous page" (and page-a (> current 1)))
     [:span {:style {:display "flex" :align-items "center" :gap "0.25rem"}}
      [:input input-attrs]
      [:span (str "/" total)]]
     (button next-a ">" "Next page" (and page-a (< current total)))
     (button last-a ">|" "Last page" (and page-a (< current total)))]))