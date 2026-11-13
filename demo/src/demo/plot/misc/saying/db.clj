
(ns saying.db)

(def sayings
  ["Peace to the world"
   "The hottentotten are here"
   "The sky is always blue"
   "Biene Maya and Willy"
   "Quot erum demunstrandum"
   "The early bird catches the worm"
   "Six Six Six"
   "Seven Angles are here"
   "8 means infinity "
   "what is the secret of nine?"])

(defn saying []
  (let [i (rand-int (count sayings))]
    (get sayings i)))
