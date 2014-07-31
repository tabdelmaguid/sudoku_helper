(ns sudoku-helper.helper
  (:require [clojure.set :as set]))

(def init-cell
  {:type :possibilities
   :value (range 1 10)})

(defn input-vals [cells]
  (->> cells
    (filter #(= (:type %) :input))
    (map :value)
    set))

(defn row-inputs [board row-index]
  (input-vals
    (board row-index)))

(defn column-inputs [board column-index]
  (input-vals
    (map #(nth % column-index) board)))

(defn subgrid-inputs [board row-index column-index]
  (let [rows (nth (partition 3 board) (quot row-index 3))]
    (reduce
      (fn [res row]
        (let [part (nth (partition 3 row) (quot column-index 3))]
          (set/union res (input-vals part))))
      #{}
      rows)))

(def all-digits (set (range 1 10)))

(defn reduce-possibilties [board row-index col-index cell]
  (cond
    (= (:type cell) :input)
    cell
    :else (let [determined-vals (set/union
                                  (row-inputs board row-index)
                                  (column-inputs board col-index)
                                  (subgrid-inputs board row-index col-index))
                remaining-digits (set/difference all-digits determined-vals)]
            (if (= 1 (count remaining-digits))
              {:type :guess
               :value (first remaining-digits)}
              {:type :possibilities
               :value remaining-digits}))))

(defn remove-used-digits [board]
  (vec
    (map-indexed
      (fn [i row]
        (vec
          (map-indexed
            (fn [j cell]
              (reduce-possibilties board i j cell))
            row)))
      board)))

(defn remove-guesses [board]
  (mapv
    #(mapv
       (fn [cell]
         (if (= (:type cell) :guess)
             init-cell
             cell))
       %)
    board))

(defn enhance-board [board]
  (-> board
    remove-guesses
    remove-used-digits))

(defn accept-input [board row col input-str]
  (enhance-board
    (assoc-in board
      [row col]
      {:type :input
       :value input-str})))

