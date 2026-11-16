(ns build
  (:require
   [clojure.tools.build.api :as b]
   [org.corfield.build :as bb]))

(def lib 'io.github.clojure-quant/itera)
(def version (format "0.1.%s" (b/git-count-revs nil)))

(defn jar [opts]
  (-> opts
      (assoc :lib lib :version version :src-pom "pom-template.xml")
      (bb/jar)))

(defn install [opts]
  (-> opts (assoc :lib lib :version version) (bb/install)))

(defn deploy [opts]
  (-> opts (assoc :lib lib :version version) (bb/deploy)))
