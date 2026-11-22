(ns itera.plot.rtable-page-editor
  "Page navigation view for the rtable page editor.

   `page-editor-view` renders first/previous/next/last page buttons plus an
   editable page-number box.  It builds the Hyper actions from the page cursor
   itself, so this namespace depends on Hyper.  With no page cursor the box is
   disabled and no actions are registered."
  (:require [hyper.core :as h]
            [itera.value.protocol :as spec-plot]))

(defn- clamp-current [total current]
  (max 1 (min (or current 1) total)))

(defn page-editor-view
  "Render page navigation for plot value V and page cursor PAGE-A.

   V supplies init-page when PAGE-A is nil.  When PAGE-A is present, it is
   deref'd for the current/total position and is mutated by the first/previous/
   next/last and jump actions.  With PAGE-A nil the box is disabled and no
   buttons or actions are registered."
  [v page-a]
  (let [page    (if page-a
                  @page-a
                  (if (satisfies? spec-plot/itera-page-plot v)
                    (spec-plot/init-page v nil nil)
                    {:page/total 1 :page/current 1}))
        total   (or (:page/total page) 1)
        current (clamp-current total (:page/current page))
        first-a  (when page-a
                   (h/action {:as "first-page"}
                     (swap! page-a assoc :page/current 1)))
        prev-a   (when page-a
                   (h/action {:as "previous-page"}
                     (swap! page-a update :page/current
                            #(max 1 (dec (clamp-current
                                          (:page/total @page-a) %))))))
        next-a   (when page-a
                   (h/action {:as "next-page"}
                     (swap! page-a update :page/current
                            #(min (:page/total @page-a)
                                  (inc (clamp-current
                                        (:page/total @page-a) %))))))
        last-a   (when page-a
                   (h/action {:as "last-page"}
                     (swap! page-a assoc :page/current
                            (or (:page/total @page-a) 1))))
        jump-a   (when page-a
                   (h/action {:as "jump-to-page"
                              :when "evt.key === 'Enter'"}
                     (when-let [n (parse-long (str $value))]
                       (swap! page-a assoc :page/current
                              (clamp-current (:page/total @page-a) n)))))
        reset-a  (when page-a
                   (h/expr (set! (.-value evt.target) ~(str current))))
        button   (fn [attr label title]
                   (when attr
                     [:button {:type "button"
                               :title title
                               :aria-label title
                               :data-on:click attr}
                      label]))
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
                      (nil? jump-a) (assoc :disabled true)
                      jump-a  (assoc :data-on:keydown jump-a)
                      reset-a (assoc :data-on:blur reset-a))]
    [:div {:style {:display "flex"
                   :align-items "center"
                   :justify-content "flex-end"
                   :gap "0.5rem"
                   :padding "0.35rem 0"
                   :font-size "0.8rem"
                   :font-family "monospace"}}
     (when (> current 1)
       (button first-a "|<" "First page"))
     (when (> current 1)
       (button prev-a "<" "Previous page"))
     [:span {:style {:display "flex" :align-items "center" :gap "0.25rem"}}
      [:input input-attrs]
      [:span (str "/" total)]]
     (when (< current total)
       (button next-a ">" "Next page"))
     (when (< current total)
       (button last-a ">|" "Last page"))]))