(ns itera.plot.pixi-timeseries.options
  "The :chartspec option editor for the pixi-timeseries viewer.

   A :chartspec option is a vector of panes, each pane a vector of plot maps
   {:type <keyword> :col <keyword|vector> :color <string>}.  The editor adds
   and removes panes and plots and edits each plot's :type, :col and :color.
   :range plots edit a 2-element :col and :bar plots edit a 4-element :col
   ([open high low close]); all other types edit a single :col.

   The option :spec carries the available columns (required) and, optionally,
   the colors; plot types always come from itera.plot.pixi.plot-type/types."
  (:require [hyper.core :as h]
            [itera.options :as options]
            [itera.plot.color :as colors]
            [itera.plot.pixi.plot-type :as plot-type]))

(def ^:private field-style
  {:width "100%"})

(def ^:private button-style
  {:font-size "0.68rem" :padding "0.15rem 0.4rem" :cursor "pointer"
   :border "1px solid #d1d5db" :border-radius "0.2rem" :background "#f3f4f6"})

(def ^:private label-style
  {:display "flex" :flex-direction "column"
   :font-size "0.68rem" :color "#374151" :gap "0.1rem"})

(defn- columns
  "The spec's required column keywords, or nil when missing/empty."
  [opt]
  (let [cols (get-in opt [:spec :columns])]
    (when (seq cols) (vec cols))))

(defn- palette
  "The spec's optional color names, defaulting to itera.plot.color/colors."
  [opt]
  (let [cs (get-in opt [:spec :colors])]
    (if (seq cs) (vec cs) (vec (keys colors/colors)))))

(defn- default-plot [cols pal]
  {:type :line :col (first cols) :color (first pal)})

(defn- ohlc-default
  "The default 4-column bar layout: [:open :high :low :close] when all exist in
   cols, otherwise the first column repeated four times."
  [cols]
  (let [want [:open :high :low :close]]
    (if (every? (set cols) want) want (vec (repeat 4 (first cols))))))

(defn- ->col-pair
  "Normalize a :col into a [c1 c2] pair, defaulting to the first column."
  [col cols]
  (let [d (first cols)
        v (if (vector? col) (vec col) [col])]
    [(or (first v) d) (or (second v) d)]))

(defn- ->col-4
  "Normalize a :col into a 4-vector for :bar, filling gaps with OHLC defaults."
  [col cols]
  (let [d (ohlc-default cols)
        v (if (vector? col) (vec col) [col])]
    [(or (nth v 0 nil) (nth d 0))
     (or (nth v 1 nil) (nth d 1))
     (or (nth v 2 nil) (nth d 2))
     (or (nth v 3 nil) (nth d 3))]))

(defn- set-plot-type
  "Change a plot's :type, normalizing :col to a 2-vector for :range, a 4-vector
   for :bar, and a single keyword otherwise."
  [plot t cols]
  (let [col (:col plot)
        col (if (nil? col) (first cols) col)]
    (cond
      (= t :range) (assoc plot :type :range :col (->col-pair col cols))
      (= t :bar)   (assoc plot :type :bar   :col (->col-4 col cols))
      :else        (assoc plot :type t
                          :col (if (vector? col) (first col) col)))))

(defn- plot-row
  "One plot's editor row."
  [{:keys [path update* cols pal types i j plot]}]
  (let [plot   (if (map? plot) plot {})
        t      (:type plot)
        col    (:col plot)
        range? (= :range t)
        bar?   (= :bar t)
        ccol   (->col-pair col cols)
        bcol   (->col-4 col cols)]
    [:div {:style {:border "1px solid #d1d5db" :border-radius "0.25rem"
                   :padding "0.35rem" :background "#fff"
                   :display "flex" :flex-direction "column" :gap "0.3rem"}}
     [:div {:style {:display "flex" :justify-content "space-between"
                    :align-items "center"}}
      [:span {:style {:font-size "0.7rem" :color "#6b7280"}}
       (str "Plot " (inc (or j 0)))]
      [:button {:type "button"
                :style (assoc button-style :background "#fee2e2")
                :data-on:click
                (h/action
                 (update* path
                          (fn [cs]
                            (let [p (vec (get cs i))]
                              (assoc cs i
                                     (vec (concat (subvec p 0 j)
                                                  (subvec p (inc j)))))))))} 
       "Remove Plot"]]
     [:label {:style label-style} "Type"
      [:select {:style field-style
                :data-on:change
                (h/action
                 (update* path
                          (fn [cs]
                            (update-in cs [i j] set-plot-type
                                       (keyword $value) cols))))}
       (for [tt types]
         [:option {:value (name tt) :selected (= tt t)} (name tt)])]]
     [:label {:style label-style} "Color"
      [:select {:style field-style
                :data-on:change
                (h/action
                 (update* path
                          (fn [cs]
                            (assoc-in cs [i j :color] $value))))}
       (for [c pal]
         [:option {:value c :selected (= c (:color plot))} c])]]
     (cond
       range?
       [:div {:style {:display "flex" :gap "0.3rem"}}
        [:label {:style (assoc label-style :flex "1")} "Col 1"
         [:select {:style field-style
                   :data-on:change
                   (h/action
                    (update* path
                             (fn [cs]
                               (let [cc (->col-pair (get-in cs [i j :col]) cols)]
                                 (assoc-in cs [i j :col]
                                           (assoc cc 0 (keyword $value)))))))}
          (for [c cols]
            [:option {:value (name c) :selected (= c (first ccol))} (name c)])]]
        [:label {:style (assoc label-style :flex "1")} "Col 2"
         [:select {:style field-style
                   :data-on:change
                   (h/action
                    (update* path
                             (fn [cs]
                               (let [cc (->col-pair (get-in cs [i j :col]) cols)]
                                 (assoc-in cs [i j :col]
                                           (assoc cc 1 (keyword $value)))))))}
          (for [c cols]
            [:option {:value (name c) :selected (= c (second ccol))} (name c)])]]]

       bar?
       [:div {:style {:display "flex" :gap "0.3rem"}}
        [:label {:style (assoc label-style :flex "1")} "Open"
         [:select {:style field-style
                   :data-on:change
                   (h/action
                    (update* path
                             (fn [cs]
                               (let [cc (->col-4 (get-in cs [i j :col]) cols)]
                                 (assoc-in cs [i j :col]
                                           (assoc cc 0 (keyword $value)))))))}
          (for [c cols]
            [:option {:value (name c) :selected (= c (nth bcol 0))} (name c)])]]
        [:label {:style (assoc label-style :flex "1")} "High"
         [:select {:style field-style
                   :data-on:change
                   (h/action
                    (update* path
                             (fn [cs]
                               (let [cc (->col-4 (get-in cs [i j :col]) cols)]
                                 (assoc-in cs [i j :col]
                                           (assoc cc 1 (keyword $value)))))))}
          (for [c cols]
            [:option {:value (name c) :selected (= c (nth bcol 1))} (name c)])]]
        [:label {:style (assoc label-style :flex "1")} "Low"
         [:select {:style field-style
                   :data-on:change
                   (h/action
                    (update* path
                             (fn [cs]
                               (let [cc (->col-4 (get-in cs [i j :col]) cols)]
                                 (assoc-in cs [i j :col]
                                           (assoc cc 2 (keyword $value)))))))}
          (for [c cols]
            [:option {:value (name c) :selected (= c (nth bcol 2))} (name c)])]]
        [:label {:style (assoc label-style :flex "1")} "Close"
         [:select {:style field-style
                   :data-on:change
                   (h/action
                    (update* path
                             (fn [cs]
                               (let [cc (->col-4 (get-in cs [i j :col]) cols)]
                                 (assoc-in cs [i j :col]
                                           (assoc cc 3 (keyword $value)))))))}
          (for [c cols]
            [:option {:value (name c) :selected (= c (nth bcol 3))} (name c)])]]]

       :else
       [:label {:style label-style} "Col"
        [:select {:style field-style
                  :data-on:change
                  (h/action
                   (update* path
                            (fn [cs]
                              (assoc-in cs [i j :col] (keyword $value)))))}
         (for [c cols]
           [:option {:value (name c) :selected (= c col)} (name c)])]])]))

(defmethod options/editor :chartspec [opt value set-fn update-fn _group]
  (let [path        (options/norm-path (:path opt))
        cols        (columns opt)
        pal         (palette opt)
        types       plot-type/types
        charts      (if (vector? value) value [])
        update*     (or update-fn (fn [p f] (set-fn p (f value))))
        add-pane    (fn [cs] (conj (vec (or cs [])) [(default-plot cols pal)]))
        remove-pane (fn [cs i]
                      (let [cs (vec cs)]
                        (vec (concat (subvec cs 0 i) (subvec cs (inc i))))))
        add-plot    (fn [cs i]
                      (update (vec (or cs [])) i (fnil conj [])
                              (default-plot cols pal)))]
    (if (nil? cols)
      [:div {:style {:color "#b91c1c" :font-size "0.75rem"}}
       "ChartSpec: the option :spec must provide a non-empty :columns vector."]
      [:div {:style {:display "flex" :flex-direction "column" :gap "0.5rem"}}
       [:div {:style {:display "flex" :justify-content "flex-end"}}
        [:button {:type "button" :style button-style
                  :data-on:click (h/action (update* path add-pane))}
         "+ Pane"]]
       (mapv
        (fn [[i pane]]
          (let [plots (if (vector? pane) pane [])]
            [:details {:open true
                       :style {:border "1px solid #e5e7eb"
                               :border-radius "0.25rem"
                               :padding "0.35rem" :background "#fafafa"}}
             [:summary {:style {:font-size "0.75rem" :font-weight "600"
                                :cursor "pointer"}}
              (str "Pane " (inc i) " (" (count plots) " plots)")]
             [:div {:style {:display "flex" :flex-direction "column"
                            :gap "0.4rem" :margin-top "0.35rem"}}
              [:div {:style {:display "flex" :gap "0.35rem"}}
               [:button {:type "button" :style button-style
                         :data-on:click
                         (h/action (update* path (fn [cs] (add-plot cs i))))}
                "+ Plot"]
               [:button {:type "button"
                         :style (assoc button-style :background "#fee2e2")
                         :data-on:click
                         (h/action (update* path (fn [cs] (remove-pane cs i))))}
                "Remove Pane"]]
              (mapv
               (fn [[j plot]]
                 (plot-row {:path path :update* update* :cols cols
                            :pal pal :types types :i i :j j :plot plot}))
               (map-indexed vector plots))]]))
        (map-indexed vector charts))])))
