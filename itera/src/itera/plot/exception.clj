(ns itera.plot.exception
  "Exception viewer (pure hiccup, no JS).

   Renders a Throwable or a dali-shaped exception map:

     (exception (ex-info \"boom\" {:x 1}))
     (exception {:message \"boom\"
                 :class \"clojure.lang.ExceptionInfo\"
                 :data {:x 1}
                 :stacktrace [{:class \"user/eval\" :method \"invoke\"
                               :file \"NO_SOURCE_FILE\" :line 1}]})

   Requiring this namespace extends java.lang.Throwable with
   itera.value.protocol/itera-convertable, so
   itera.value.converter/type->hiccup renders exceptions as well."
  (:require [itera.value.protocol]))

(def head [])

(defn- stack-frame [^StackTraceElement frame]
  {:class  (.getClassName frame)
   :method (.getMethodName frame)
   :file   (.getFileName frame)
   :line   (.getLineNumber frame)})

(defn exception->data
  "Normalize a Throwable or an already dali-shaped map to
   {:message :class :data :stacktrace}."
  [ex]
  (cond
    (map? ex) (select-keys ex [:message :class :data :stacktrace])
    (instance? Throwable ex)
    (let [^Throwable ex ex]
      {:message    (ex-message ex)
       :class      (some-> (class ex) .getName)
       :data       (ex-data ex)
       :stacktrace (mapv stack-frame (.getStackTrace ex))})
    :else {:message nil :class nil :data nil :stacktrace nil}))

(def ^:private monospace
  "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, monospace")

(def ^:private container-style
  {:background "linear-gradient(135deg, #fff5f5 0%, #ffe4e6 100%)"
   :border "1px solid #fda4af"
   :border-radius "12px"
   :padding "16px"
   :color "#7f1d1d"
   :box-shadow "0 8px 24px rgba(136, 19, 55, 0.12)"
   :margin "10px 0"
   :max-width "60em"
   :width "100%"})

(def ^:private title-style
  {:font-size "16px"
   :font-weight 700
   :margin "0 0 8px 0"})

(def ^:private message-style
  {:margin "0 0 10px 0"
   :font-size "14px"})

(def ^:private class-style
  {:margin "0 0 12px 0"
   :font-family monospace
   :font-size "13px"
   :color "#9f1239"})

(def ^:private data-style
  {:margin "0 0 12px 0"
   :padding "10px"
   :background "#fff"
   :border "1px solid #fecdd3"
   :border-radius "8px"
   :font-family monospace
   :font-size "12px"
   :white-space "pre-wrap"
   :word-break "break-word"})

(def ^:private summary-style
  {:color "#9f1239"
   :font-size "13px"
   :font-weight 600
   :cursor "pointer"})

(def ^:private table-style
  {:width "100%"
   :margin-top "12px"
   :border-collapse "collapse"
   :background "#ffffff"
   :border "1px solid #fecdd3"
   :border-radius "8px"
   :overflow "hidden"})

(def ^:private cell-style
  {:padding "8px 10px"
   :font-family monospace
   :font-size "13px"
   :line-height "1.35"
   :vertical-align "top"})

(def ^:private row-style
  {:border-top "1px solid #e5e7eb"})

(defn- stacktrace-line [{:keys [class method file line]}]
  [:tr {:style row-style}
   [:td {:style (merge cell-style {:color "#1e3a8a" :font-weight "600"})} class]
   [:td {:style (merge cell-style {:color "#1e3a8a"})} method]
   [:td {:style (merge cell-style {:color "#1e3a8a"})} (str file ": " line)]])

(defn exception->hiccup [{:keys [message data class stacktrace]}]
  [:div {:style container-style}
   [:p {:style title-style} "Exception"]
   [:p {:style message-style} (or message "Unknown error")]
   [:p {:style class-style} (or class "Unknown class")]
   (when data
     [:pre {:style data-style} (pr-str data)])
   (when (seq stacktrace)
     [:details
      [:summary {:style summary-style} "Show stacktrace"]
      [:table {:style table-style}
       [:tbody (map stacktrace-line stacktrace)]]])])

(defrecord ExceptionView [exception]
  itera.value.protocol/itera-convertable
  (to-hiccup [v]
    (exception->hiccup (exception->data (:exception v)))))

(defn exception
  "Return an exception viewer for a Throwable or dali-shaped map."
  [exception]
  (ExceptionView. exception))

(extend-protocol itera.value.protocol/itera-convertable
  java.lang.Throwable
  (to-hiccup [v]
    (exception->hiccup (exception->data v))))

(comment
  (require '[itera.value.converter :as converter])
  (exception->data (IllegalArgumentException. "bad"))
  (itera.value.protocol/to-hiccup (exception (ex-info "boom" {:x 1})))
  (converter/type->hiccup (ex-info "boom" {:x 1}))
  
  
  ;
  )