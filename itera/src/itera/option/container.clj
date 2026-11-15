(ns itera.option.container
  "The options editor for :container windows.

   Registers the `itera.options/editor` method dispatched on :type :container,
   the measured/fixed window editor (:mode :container|:fixed, :width, :height).

   This namespace is not required by itera.container (that would be circular);
   it is loaded from the itera project's `itera-plots.edn` :option-ui entry by
   `itera.hyper/start`."
  (:require [clojure.string :as str]
            [hyper.core :as h]
            [itera.container :as container]
            [itera.options :as options]))

;; --- helpers ----------------------------------------------------------------

(def ^:private field-style {:width "100%"})

(def ^:private label-style
  {:display "flex" :flex-direction "column"
   :font-size "0.68rem" :color "#374151" :gap "0.1rem"})

(defn- ->int
  "Parse a number input value; nil for blank/invalid/non-positive input."
  [v]
  (let [s (some-> v str str/trim)]
    (when (seq s)
      (let [n (parse-long s)]
        (when (and n (pos? n)) n)))))

;; --- editor -----------------------------------------------------------------

(defmethod options/editor :container [opt value set-fn update-fn _group]
  (let [path     (options/norm-path (:path opt))
        w        (container/normalize-window value)
        mode     (:mode w)
        fixed?   (= :fixed mode)
        update*  (or update-fn (fn [p f] (set-fn p (f value))))
        set-mode (fn [target]
                   (fn [w]
                     (let [w (container/normalize-window w)]
                       (if (= :fixed target)
                         {:mode :fixed
                          :width (or (:width w) 800)
                          :height (or (:height w) 600)}
                         {:mode :container}))))]
    [:div {:class (:class opt)
           :style (merge {:display "flex" :flex-direction "column" :gap "0.35rem"}
                         (:style opt))}
     [:label {:style label-style} "Mode"
      [:select {:style field-style
                :data-on:change
                (h/action
                 (update* path (set-mode (keyword $value))))}
       [:option {:value "container" :selected (not fixed?)} "container"]
       [:option {:value "fixed" :selected fixed?} "fixed"]]]
     (if fixed?
       [:div {:style {:display "flex" :gap "0.35rem"}}
        [:label {:style (assoc label-style :flex "1")} "Width"
         [:input {:type "number" :min 1 :step 1 :style field-style
                  :value (str (or (:width w) ""))
                  :data-on:change
                  (h/action
                   (update* path
                            (fn [w]
                              (let [w (container/normalize-window w)
                                    n (->int $value)]
                                (if n (assoc w :width n) w)))))}]]
        [:label {:style (assoc label-style :flex "1")} "Height"
         [:input {:type "number" :min 1 :step 1 :style field-style
                  :value (str (or (:height w) ""))
                  :data-on:change
                  (h/action
                   (update* path
                            (fn [w]
                              (let [w (container/normalize-window w)
                                    n (->int $value)]
                                (if n (assoc w :height n) w)))))}]]]
       [:div {:style {:font-size "0.68rem" :color "#6b7280"}}
        (str "Measured: "
             (or (:width w) "-") " x " (or (:height w) "-")
             " px")])]))