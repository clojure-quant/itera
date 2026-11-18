(ns itera.page.overview
  (:require [clojure.string :as str]
            [hyper.core :as h]))

(defn- split-name
  "Split an alg name \"<ns>/<symbol>\" into {:ns <ns> :symbol <symbol>}."
  [name]
  (let [[n s] (str/split name #"/" 2)]
    {:ns n :symbol s}))

(defn- strip-prefix
  "Remove prefix from s, but only when s is longer than the prefix itself."
  [prefix s]
  (if (and (str/starts-with? s prefix)
           (> (count s) (count prefix)))
    (subs s (count prefix))
    s))

(defn- ns-parts
  "Split a namespace into display segments, dropping a trailing \"itera\"
   segment when present."
  [ns-name]
  (let [parts (vec (str/split ns-name #"\."))]
    (if (and (seq parts) (= "itera" (peek parts)))
      (let [stripped (pop parts)]
        (if (seq stripped) stripped ["itera"]))
      parts)))

(defn- insert-algo
  "Insert entry into the trie at namespace path segments parts."
  [node parts entry]
  (if (empty? parts)
    (update node :algos conj entry)
    (let [part  (first parts)
          child (get-in node [:children part]
                        {:label part :children {} :algos []})]
      (assoc-in node [:children part]
                (insert-algo child (rest parts) entry)))))

(defn- build-tree
  "Build a namespace trie from discovery entries."
  [algos]
  (reduce
   (fn [root {:keys [name]}]
     (let [{:keys [ns symbol]} (split-name name)]
       (insert-algo root (ns-parts ns)
                    {:name   name
                     :ns     ns
                     :symbol symbol
                     :label  (strip-prefix "algo-" symbol)})))
   {:children {} :algos []}
   algos))

(defn- sorted-by-label
  "Sort items case-insensitively by their display :label."
  [xs]
  (sort-by (fn [x] (str/lower-case (str (:label x)))) xs))

(defn- node-view
  "Render one trie node: its namespace label, its algo links, and its
   child namespaces."
  [node]
  [:li {:key (:label node)}
   [:span.ns-label (:label node)]
   (when (seq (:algos node))
     [:span.algo-links
      (for [{:keys [label name ns symbol]} (sorted-by-label (:algos node))]
        [:a (merge {:key name :style {:margin-left "0.75rem"}}
                   (h/navigate :itera-algo {:ns ns :symbol symbol}))
         label])])
   (when (seq (:children node))
     [:ul {:style {:list-style-type "none" :padding-left "1.25rem"}}
      (for [child (sorted-by-label (vals (:children node)))]
        (node-view child))])])

(defn- tree-view
  "Render the root of the namespace trie."
  [root]
  [:ul {:style {:list-style-type "none" :padding-left "0"}}
   (for [child (sorted-by-label (vals (:children root)))]
     (node-view child))])

(defn overview-page
  "Form-2 view: subscribes to the discover result atom so the page re-renders
   when itera.clj files change."
  [result*]
  (fn [req]
    (h/watch! result*)
    (fn [req]
      (let [{:keys [algos error]} @result*
            tree (build-tree algos)]
        [:div.page
         [:div {:style {:display "flex" :align-items "baseline" :gap "1rem"}}
          [:h1 "Itera"]
          [:a (merge {:style {:font-weight "600"}}
                     (h/navigate :itera-graph))
           "Graph editor"]
          [:a (merge {:style {:font-weight "600"}}
                     (h/navigate :itera-model-list))
           "Models"]]
         (tree-view tree)
         (when (seq error)
           [:div.errors
            [:h2 "Errors"]
            [:table
             [:thead [:tr [:th "file"] [:th "error"]]]
             [:tbody
              (for [{:keys [file error]} error]
                [:tr {:key file} [:td file] [:td (str error)]])]]])]))))
