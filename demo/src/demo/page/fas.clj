(ns demo.page.fas
  "Font Awesome reference page: every free solid icon in the Font Awesome
   6.5.1 snapshot, grouped A-Z with labels, aliases, and a server-side filter.

   The icon list in `public/data/fontawesome-solid.edn` and the stylesheet pin in
   itera.util.fas must be regenerated together when the Font Awesome version
   changes."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hyper.core :as h]
            [itera.util.fas :as fas]))

(def ^:private icons*
  (delay (edn/read-string (slurp (io/resource "public/data/fontawesome-solid.edn")))))

(def ^:private page-css
  ".fas-grid {display:grid;grid-template-columns:repeat(auto-fill,minmax(160px,1fr));gap:0.5rem;}
.fas-card {border:1px solid #e5e7eb;border-radius:0.375rem;padding:0.5rem;display:flex;flex-direction:column;align-items:center;gap:0.25rem;text-align:center;min-height:6rem;}
.fas-card .fa-solid {font-size:1.5rem;width:1.5em;text-align:center;}
.fas-name {font-family:ui-monospace,SFMono-Regular,Menlo,monospace;font-size:0.75rem;word-break:break-all;}
.fas-label {font-size:0.75rem;color:#6b7280;}
.fas-alias {font-size:0.7rem;color:#9ca3af;}
.fas-group {position:sticky;top:0;background:#fff;padding:0.5rem 0;margin:1rem 0 0.5rem;z-index:2;border-bottom:1px solid #e5e7eb;}
.fas-jump {display:flex;flex-wrap:wrap;gap:0.35rem;margin:0.75rem 0;}
.fas-jump a {font-size:0.8rem;font-weight:600;text-decoration:none;color:#2563eb;padding:0.05rem 0.3rem;border:1px solid #dbeafe;border-radius:0.25rem;}
.fas-snippet {background:#f6f6f6;padding:0.5rem 0.75rem;border-radius:0.375rem;margin:0.5rem 0;}")

(defn- group-key
  "A-Z bucket for an icon name; anything not starting with a letter goes to #."
  [nm]
  (let [c (str (first nm))]
    (if (re-matches #"[A-Za-z]" c) (str/lower-case c) "#")))

(defn- group-anchor
  [k]
  (str "fas-group-" (if (= k "#") "num" k)))

(defn- matches?
  "Case-insensitive substring match across name, label, and aliases."
  [q icon]
  (let [q (str/lower-case (str/trim (or q "")))]
    (if (str/blank? q)
      true
      (let [{:keys [name label aliases]} icon
            haystack (str/lower-case
                      (str/join " " (concat [name label (str "fa-" name)]
                                            aliases
                                            (map #(str "fa-" %) aliases))))]
        (str/includes? haystack q)))))

(defn- group-icons
  "Group icons by first letter, preserving the input order within each group."
  [icons]
  (let [groups (group-by (comp group-key :name) icons)]
    (for [k (sort (keys groups))]
      [k (get groups k)])))

(defn- icon-card
  [{:keys [name label aliases]}]
  [:div {:key name :class "fas-card"}
   (fas/fas (keyword (str "fa-" name)))
   [:code {:class "fas-name"} (str "fa-" name)]
   [:div {:class "fas-label"} label]
   (when (seq aliases)
     [:div {:class "fas-alias"} "aka " (str/join ", " aliases)])])

(defn page-fas
  "Form-2 view over a tab cursor holding the filter string.  Renders the full
   icon reference; each card calls `fas`, so the first render also marks the
   Font Awesome stylesheet for this tab."
  [_ctx]
  (fn [_req]
    (let [all      (force icons*)
          search-a (h/tab-cursor [:fontawesome-demo :search] "")
          q        @search-a
          shown    (filterv #(matches? q %) all)
          groups   (group-icons shown)]
      [:div.page {:style {:padding "1rem" :max-width "72rem" :margin "0 auto"}}
       [:style page-css]
       [:h1 "Font Awesome Solid 6.5.1"]
       [:p {:style {:color "#4b5563" :margin "0.25rem 0"}}
        (str (count all) " free solid icons. Served from the cdnjs 6.5.1 CSS pin; "
             "the icon list is a point-in-time snapshot, so regenerate both when "
             "the version changes.")]
       [:pre {:class "fas-snippet"} "(fas :fa-user-nurse)"]
       [:div {:style {:display "flex" :gap "1rem" :align-items "center"
                      :flex-wrap "wrap" :margin "0.75rem 0"}}
        [:input {:type "text"
                 :value (str q)
                 :placeholder "Filter (press Enter)"
                 :aria-label "Filter icons"
                 :style {:font-size "0.9rem"
                         :padding "0.35rem 0.6rem"
                         :border "1px solid #cbd5e1"
                         :border-radius "0.375rem"
                         :min-width "18rem"}
                 :data-on:change (h/action (reset! search-a $value))}]
        [:span {:style {:font-size "0.85rem" :color "#6b7280"}}
         (str "Showing " (count shown) " of " (count all) " icons")]]
       [:div {:class "fas-jump"}
        (for [[k _] groups]
          [:a {:key k :href (str "#" (group-anchor k))}
           (if (= k "#") "#" (str/upper-case k))])]
       (when (empty? shown)
         [:p {:style {:color "#6b7280"}} "No icons match that filter."])
       (for [[k items] groups]
         [:section {:key k}
          [:h2 {:id (group-anchor k) :class "fas-group"}
           (if (= k "#") "#" (str/upper-case k))]
          [:div {:class "fas-grid"}
           (for [icon items]
             (icon-card icon))]])])))