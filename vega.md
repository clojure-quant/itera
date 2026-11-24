


in specs :data is 
- either :values and a vector or
- :url and a string.

if :datasets is specified, it is a map
of name - value.
then :data can be {:name "square"} to resolve the
correct dataset.

data can also be generated:
{:sequence {:as "x", :start 0, :step 0.1, :stop 12.7}}

for geojson it can have:
{:format {:feature "countries", :type "topojson"}, 
 :name "world", 
 :url "data/world-110m.json"},
 
:sphere true; 
:data may also be omitted on nested views.


"data": {
    "values": [
      {"index": 1, "nav": 100},
      {"index": 2, "nav": 120},
      {"index": 3, "nav": 150},
      {"index": 4, "nav": 120},
      {"index": 5, "nav": 140},
      {"index": 6, "nav": 150},
      {"index": 7, "nav": 160},
      {"index": 8, "nav": 150},
      {"index": 9, "nav": 172.5}]

"data": {"url": "data/stocks.csv"},
