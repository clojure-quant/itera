(ns itera.runner.formula
  (:require
   [itera.util.clj :as util]))

(defn create-formula [{:keys [value code]}]
  (cond
    ; flow "variable"
    (and value (-> value meta :itera/output))
    {:value value
     :flow? true}
    ; function with args 
    (and value (fn? value))
    (let [args (util/fn-arglist code)]
      {:fn value
       :args (->> args
                  (remove #(contains? #{'_ 'ctx} %))
                  (map name)
                  (into []))
       :ctx? (->> args
                  (some #(contains? #{'_ 'ctx} %)))})
    
    ))


(defn args-val [{:keys [args]} available-arg-dict]
  (when (->> args
             (map (fn [arg]
                    (contains? available-arg-dict arg)))
             (reduce (fn [a b] (and a b)) true))
    (->> args 
         (map (fn [arg]
                (get available-arg-dict arg)
                ))
         (into []))))

(defn run-formula [ctx {:keys [fn ctx?]} args-vec]
  (cond
    (and ctx? args-vec)
    (apply fn ctx args-vec)

    :else
    (apply fn args-vec)))



(comment
  (def ctx {})
  (def dag-ns "my.bongo.3")
  
  (args-val {:args ["a" "b"]} {"b" 16 "a" 3 "c" 5})
  (args-val {:args ["a" "b"]} {"b" 16 "A" 3 "c" 5})


  (-> (util/compile-formula dag-ns "(fn [ctx assets] (println assets))")
      (create-formula))
  ;;=> {:fn #function[my.bongo.3/eval16262/fn--16263], :args ["assets"], :ctx? true}
  
  (-> (util/compile-formula dag-ns "(fn [_ assets] (println assets))")
      (create-formula))
  ;;=> {:fn #function[my.bongo.3/eval16270/fn--16271], :args ["assets"], :ctx? true}
  
  (-> (util/compile-formula dag-ns "(fn [assets] (println assets))")
      (create-formula))
  ;;=> {:fn #function[my.bongo.3/eval16278/fn--16279], :args ["assets"], :ctx? nil}
  

  (def f (-> (util/compile-formula dag-ns "(fn [a b] (+ a b))")
             (create-formula)))
  f
  
  (def f (-> (util/compile-formula dag-ns "34")
           (create-formula)))
f
  
  (def f (-> (util/compile-formula dag-ns "df/dt-flow")
            (create-formula)))
  f

  (let [ctx {}
        args-vec (args-val f {"a" 1 "b" 10})]
    (run-formula ctx f args-vec)
    )
  

 ; 
  )




