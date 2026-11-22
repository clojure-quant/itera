(ns itera.value.protocol)


(defprotocol itera-convertable
  (to-hiccup [v]))


(defprotocol itera-spec-plot
  (to-formatted-hiccup [v spec window])
  (default-spec [v])
  (spec-editor [v spec code-a]))


(defn window->style
  "CSS style from a container dimension {:width <px> :height <px>}."
  [window]
  (when window
    (cond-> {}
      (:width window)  (assoc :width  (str (:width window) "px"))
      (:height window) (assoc :height (str (:height window) "px")))))

(defprotocol itera-page-plot
  (to-formatted-paged-hiccup [data spec window page])
  (init-page [data spec window])
  (page-editor [data spec window page-a]))


(comment 
  (defrecord Hiccup [data]
    itera-spec-plot
    (to-formatted-hiccup [v spec container-dimension]
               {:data (:data v) :spec spec :container-dimension container-dimension})
    (default-spec [v] :default-spec)
    (spec-editor [v spec code-a]
      :hiccup-editor))
  
  (def x (Hiccup. :data))
  (to-formatted-hiccup x :rows :big)
  
 ; 
  )