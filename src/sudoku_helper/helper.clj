(ns sudoku-helper.helper
  (:require [clojure.set :as set]))

(def init-cell
  {:type :possibilities
   :value (range 1 10)})

(defn known-vals [cells]
  (->> cells
    (filter #(#{:input :guess} (:type %)))
    (map :value)
    set))

(defn row-knowns [board row-index]
  (known-vals
    (board row-index)))

(defn column-knowns [board column-index]
  (known-vals
    (map #(nth % column-index) board)))

(defn subgrid-knowns [board row-index column-index]
  (let [rows (nth (partition 3 board) (quot row-index 3))]
    (reduce
      (fn [res row]
        (let [part (nth (partition 3 row) (quot column-index 3))]
          (set/union res (known-vals part))))
      #{}
      rows)))

(def all-digits (set (range 1 10)))

(defn reduce-possibilties [board row-index col-index cell]
  (cond
    (= (:type cell) :input)
      cell
    :else (let [determined-vals (set/union
                                  (row-knowns board row-index)
                                  (column-knowns board col-index)
                                  (subgrid-knowns board row-index col-index))
                remaining-digits (set/difference all-digits determined-vals)]
            (cond
              (= 0 (count remaining-digits))
                cell
              (= 1 (count remaining-digits))
                {:type :guess
                 :value (first remaining-digits)}
              :else
                {:type :possibilities
                 :value remaining-digits}))))

(defn remove-known-digits [board]
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

(defn guess-single-show [board]
  board)

(defn iterate-until-no-change [board fun]
  (let [new-board (fun board)]
    (if (= new-board board)
      board
      (recur new-board fun))))

(defn enhance-board-step [board]
  (-> board
    remove-known-digits))

(defn enhance-board [board]
  (-> board
    remove-guesses
    (iterate-until-no-change enhance-board-step)))

(defn accept-input [board row col input-str]
  (enhance-board
    (assoc-in board
      [row col]
      {:type :input
       :value input-str})))

