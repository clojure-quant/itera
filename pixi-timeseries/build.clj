(ns build
  (:require
   [clojure.tools.build.api :as b]
   [org.corfield.build :as bb]))

(def lib 'io.github.clojure-quant/itera-pixi-timeseries)
(def itera-lib 'io.github.clojure-quant/itera)
(def version (format "0.1.%s" (b/git-count-revs nil)))

(defn- basis []
  (b/create-basis
   {:extra {:aliases {:clojars {:override-deps {itera-lib {:mvn/version version}}}}}
    :aliases [:clojars]}))

(defn jar [opts]
  (-> opts
      (assoc :lib lib :version version :src-pom "pom-template.xml" :basis (basis))
      (bb/jar)))

(defn install [opts]
  (-> opts (assoc :lib lib :version version :basis (basis)) (bb/install)))

(defn deploy [opts]
  (-> opts (assoc :lib lib :version version) (bb/deploy)))
