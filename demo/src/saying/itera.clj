(ns saying.itera
  (:require
   [saying.db :as db]))

(defn calculate [_ _]
  (db/saying))

(defn plot [_ s]
  [:div
   [:blockquote
    {:style {:fontSize "1.4rem"
             :fontFamily "cursive, Georgia, serif"
             :margin "0.75rem 0 0 0"
             :padding "1rem 1.25rem"
             :borderLeft "4px solid #6b9bd1"
             :backgroundColor "#eef6fc"
             :borderRadius "0 0.375rem 0.375rem 0"
             :lineHeight 1.5}}
    s]])


(def algo-saying
  {:calculate/fn calculate
   :plot/fn plot})
