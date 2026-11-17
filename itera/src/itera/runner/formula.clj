(ns itera.runner.formula
  (:require
   [clojure.string :as string]
   [clojure.edn :as edn]
   [clojure.core :refer [read-string load-string]])
  (:import
   [clojure.lang Compiler$CompilerException]))

(defmacro with-out-str-data-map
  [& body]
  `(let [s# (new java.io.StringWriter)]
     (binding [*out* s#]
       (let [r# ~@body]
         {:value r#
          :out    (str s#)} ; nrepl compatible!
         ))))

(defn root-cause [^Throwable t]
  (if-some [c (.getCause t)]
    (recur c)
    t))

(defn compiler-exception [ex]
  (let [msg (ex-message (root-cause ex))
        ^Throwable e (ex-info msg {})]
    (.setStackTrace e (into-array StackTraceElement []))
    e))

(defn clj-eval-raw [code]
  (try
    (with-out-str-data-map (load-string code))
    (catch Compiler$CompilerException ex  ;Exception ex
      {:ex ;ex
       (compiler-exception ex)})))

(defn clj-eval
  "evaluate code in namespace ns
   if no ns is passed, it will execute the code in the current ns (*ns*)
   returns the result of the last form in code.
   (it would be very easy to return all results as well)"
  [{:keys [code ns]}]
  (let [code-ns (if (and ns (not (string/blank? ns)))
                  (str "(in-ns '" ns " ) ")
                  "")
       ; _ (error "code-to-set-ns: " code-ns)
        ;code-with-ns (str code-ns " [ " code " (str *ns*) ]")
        code-with-ns (str code-ns code)
        er-code (clj-eval-raw code-with-ns)
       ; _ (error "er-code: " er-code)
        ; [[nil] "notebook.study.movies"]
        {:keys [value]} er-code
        ;ns-after (if (:err er-code)
        ;           ns  ; on compile exception the ns does not change
        ;           (last value))
        ;values-new (drop-last value)
        ;last-value (last values-new)
        ;er-code (clj-eval-raw code)
        ;er-ns-after (clj-eval-raw "*ns*")
        ;ns-after  (-> er-ns-after :value str) ; (str *ns*)
        ;last-value value
        ;new-ns (:value (clj-eval-raw "(str *ns*)"))
        r (merge er-code
                 {:code code
                  :ns ns ; new-ns  ;:ns ns-after
                  })]
    ;(info "eval-result: " r)
    r))


(defn create-workbook [workbook-ns requires]
  (clj-eval-raw
   (if (seq requires)
     (str "(ns " workbook-ns " (:require "
          (string/join "\n  " (map pr-str requires))
          "))")
     (str "(ns " workbook-ns ")"))))

(defn compile-formula [workbook-ns code]
  (clj-eval {:code code :ns workbook-ns}))


(defn fn-arglist
  "Reads a single form from s and returns the parameter vector
   of an (fn [args] ...) form."
  [s]
  (let [form (edn/read-string s)]
    (when (and (seq? form) (= 'fn (first form)))
      (second form))))

(defn create-formula [{:keys [value code]}]
  (cond
    ; flow "variable"
    (and value (-> value meta :itera/output))
    {:value value
     :flow? true}
    ; function with args 
    (and value (fn? value))
    (let [args (fn-arglist code)]
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


  (create-workbook dag-ns '[[clojure.string :as string]
                            [clojure.edn :as edn]
                            [demo.flow :as df]])

  (-> (compile-formula dag-ns "(fn [ctx assets] (println assets))")
      (create-formula))
  ;;=> {:fn #function[my.bongo.3/eval16262/fn--16263], :args ["assets"], :ctx? true}
  
  (-> (compile-formula dag-ns "(fn [_ assets] (println assets))")
      (create-formula))
  ;;=> {:fn #function[my.bongo.3/eval16270/fn--16271], :args ["assets"], :ctx? true}
  
  (-> (compile-formula dag-ns "(fn [assets] (println assets))")
      (create-formula))
  ;;=> {:fn #function[my.bongo.3/eval16278/fn--16279], :args ["assets"], :ctx? nil}
  

  (def f (-> (compile-formula dag-ns "(fn [a b] (+ a b))")
             (create-formula)))
  f
  
  (def f (-> (compile-formula dag-ns "34")
           (create-formula)))
f
  
  (def f (-> (compile-formula dag-ns "df/dt-flow")
            (create-formula)))
  f

  (let [ctx {}
        args-vec (args-val f {"a" 1 "b" 10})]
    (run-formula ctx f args-vec)
    )
  

 ; 
  )




