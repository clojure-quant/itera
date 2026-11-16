(ns demo.graph
  "Example node catalog and initial graph for the /itera/graph editor.

   The catalog deliberately mixes port counts and data types: 0-input and
   0-output nodes, multi-input nodes, a two-output node, :any ports, and a
   :number port that is incompatible with :dataset ports.")

(def node-types
  [{:type :load-csv
    :label "Load CSV"
    :inputs []
    :outputs [{:id "out" :label "Data" :data-type :dataset}]
    :fields [{:key :path :label "Path" :type :text
              :default "demo/resources/data/stocks.csv"}]}
   {:type :sma
    :label "SMA"
    :inputs [{:id "data" :label "Data" :data-type :dataset}]
    :outputs [{:id "out" :label "SMA" :data-type :dataset}]
    :fields [{:key :window :label "Window" :type :number :default 20}]}
   {:type :join
    :label "Join"
    :inputs [{:id "left" :label "Left" :data-type :dataset}
             {:id "right" :label "Right" :data-type :dataset}]
    :outputs [{:id "out" :label "Joined" :data-type :dataset}]
    :fields [{:key :on :label "On" :type :text :default "date"}]}
   {:type :merge-3
    :label "Merge 3"
    :inputs [{:id "a" :label "A" :data-type :any}
             {:id "b" :label "B" :data-type :any}
             {:id "c" :label "C" :data-type :any}]
    :outputs [{:id "out" :label "Merged" :data-type :any}]
    :fields []}
   {:type :split
    :label "Split"
    :inputs [{:id "data" :label "Data" :data-type :dataset}]
    :outputs [{:id "train" :label "Train" :data-type :dataset}
              {:id "test" :label "Test" :data-type :dataset}]
    :fields [{:key :ratio :label "Ratio" :type :number :default 0.8}]}
   {:type :plot
    :label "Plot"
    :inputs [{:id "data" :label "Data" :data-type :any}]
    :outputs []
    :fields [{:key :title :label "Title" :type :text :default "Price"}
             {:key :kind :label "Kind" :type :select
              :options ["line" "bar"] :default "line"}]}
   {:type :const-number
    :label "Number"
    :inputs []
    :outputs [{:id "out" :label "Number" :data-type :number}]
    :fields [{:key :value :label "Value" :type :number :default 1}]}])

(def initial-graph
  {:nodes [{:id "n-load" :type :load-csv
            :position {:x 60 :y 120}
            :fields {:path "demo/resources/data/stocks.csv"}}
           {:id "n-sma" :type :sma
            :position {:x 420 :y 120}
            :fields {:window 20}}
           {:id "n-plot" :type :plot
            :position {:x 780 :y 120}
            :fields {:title "Price"}}]
   :edges [{:id "e-1" :source-node "n-load" :source-port "out"
            :target-node "n-sma" :target-port "data"}
           {:id "e-2" :source-node "n-sma" :source-port "out"
            :target-node "n-plot" :target-port "data"}]})