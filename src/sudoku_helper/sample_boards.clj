(def board1
  '[ [ 3 - - - - 5 - 1 - ]
     [ - 7 - - - 6 - 3 - ]
     [ 1 - - - 9 - - - - ]
     [ 7 - 8 - - - - 9 - ]
     [ 9 - - 4 - 8 - - 2 ]
     [ - 6 - - - - 5 - 1 ]
     [ - - - - 4 - - - 6 ]
     [ - 4 - 7 - - - 2 - ]
     [ - 2 - 6 - - - - 3 ] ])

(defn to-cell [ch]
  (if (integer? ch)
    {:type :input
     :value ch}
    {:type :possibilities
     :value (range 1 10)}))

(defn to-cell-board [flat-board]
  (mapv
    #(mapv to-cell %)
    board1))
