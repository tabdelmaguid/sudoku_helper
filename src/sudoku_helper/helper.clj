(ns sudoku-helper.helper
  (:require [clojure.set :as set]))

(def cell-range (range 9))

(def all-possibilities (set (range 1 10)))

(def init-cell
  {:type :possibilities
   :value all-possibilities})

(defn guess-cell-of [val]
  {:type :guess
   :value val})

(defn available-cell-of [possibilities]
  {:type :possibilities
   :value possibilities})

(defn known-cell? [cell]
  (#{:input :guess} (:type cell)))

(defn map-on-board [board fun]
  (mapv #(mapv fun %) board))

(defn iterate-until-no-change [board fun]
  (let [new-board (fun board)]
    (if (= new-board board)
      board
      (recur new-board fun))))

(defn cell-value-str [cell]
  (format "%9s"
    (if (known-cell? cell)
      (:value cell)
      (apply str (:value cell)))))

(defn print-board-vals [board]
  (let [board-vals (map-on-board board cell-value-str)
        with-nl (interpose "\n" board-vals)]
    (println with-nl)))

(defn known-vals [cells]
  (->> cells
    (filter known-cell?)
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

(defn collect-determined-vals-around [board row-index column-index]
  (set/union
    (row-knowns board row-index)
    (column-knowns board column-index)
    (subgrid-knowns board row-index column-index)))

(defn reduce-possibilties [board row-index column-index cell]
  (cond
    (known-cell? cell)
      cell
    :else (let [determined-vals (collect-determined-vals-around board row-index column-index)
                remaining-digits (set/difference all-possibilities determined-vals)]
            (cond
              (= 0 (count remaining-digits))
                cell
              (= 1 (count remaining-digits))
                (guess-cell-of (first remaining-digits))
              :else
                (available-cell-of remaining-digits)))))

(defn remove-known-digits-step [board]
  (vec
    (map-indexed
      (fn [i row]
        (vec
          (map-indexed
            (fn [j cell]
              (reduce-possibilties board i j cell))
            row)))
      board)))

(defn remove-known-digits [board]
  (iterate-until-no-change board remove-known-digits-step))

(defn remove-guesses [board]
  (mapv
    #(mapv
       (fn [cell]
         (if (= (:type cell) :guess)
             init-cell
             cell))
       %)
    board))

(defn cell-indexes-matching [section pred]
  (second
    (reduce
      (fn [[index result] val]
        [(inc index) (if (pred val)
                       (conj result index)
                       result)])
      [0 #{}]
      section)))

(defn cell-indexes-containing [section val]
  (cell-indexes-matching
    section
    (fn [cell]
      (and
        (= (:type cell) :possibilities)
        ((:value cell) val)))))

(defn guess-single-show-on-section [section]
  (let [value-in-cells
         (map
           (fn [val] (cell-indexes-containing section val))
           all-possibilities)
        values-to-indexes (zipmap all-possibilities value-in-cells)
        values-in-unique-cells
          (filter
            (fn [[val indexes]] (= 1 (count indexes)))
            values-to-indexes)]
    (reduce
      (fn [section [val cells-indexes]]
        (assoc-in section [(first cells-indexes)] (guess-cell-of val)))
      section
      values-in-unique-cells)))

(defn guess-single-show-on-rows [board]
  (mapv
    #(guess-single-show-on-section %)
    board))

(defn get-column [board column-index]
  (vec (nth (apply map vector board) column-index)))

(defn set-col [board col col-index]
  (reduce
    (fn [board row-index]
      (assoc-in board [row-index col-index] (col row-index)))
    board
    cell-range))

(defn guess-single-show-on-col [board col-index]
  (let [col (get-column board col-index)
        guessed-col (guess-single-show-on-section col)]
    (set-col board guessed-col col-index)))

(defn guess-single-show-on-columns [board]
  (reduce
    (fn [board index]
      (guess-single-show-on-col board index))
    board
    cell-range))

(defn get-subgrid [board index]
  (let [subgrid-row-index (quot index 3)
        subgrid-col-index (mod index 3)
        subgrid-rows (nth (partition 3 board) subgrid-row-index)]
    (vec
      (mapcat
        #(nth (partition 3 %) subgrid-col-index)
        subgrid-rows))))

(defn set-subgrid [board subgrid index]
  (reduce
    (fn [board cell-index]
      (let [row (+ (* 3 (quot index 3)) (quot cell-index 3))
            col (+ (* 3 (mod index 3)) (mod cell-index 3))]
        (assoc-in board [row col] (subgrid cell-index))))
    board
    cell-range))

(defn guess-single-show-on-subgrid [board index]
  (let [subgrid (get-subgrid board index)
        guessed-subgrid (guess-single-show-on-section subgrid)]
    (set-subgrid board guessed-subgrid index)))

(defn guess-single-show-on-subgrids [board]
  (reduce
    (fn [board index]
      (guess-single-show-on-subgrid board index))
    board
    cell-range))

(defn guess-single-show [board]
  (-> board
    remove-known-digits
    guess-single-show-on-rows
    remove-known-digits
    guess-single-show-on-columns
    remove-known-digits
    guess-single-show-on-subgrids))

(defn enhance-board-step [board]
  (-> board
    guess-single-show))

(defn all-rows [board] board)

(defn all-columns [board]
  (for [index cell-range]
    (get-column board index)))

(defn all-subgrids [board]
  (for [index cell-range]
    (get-subgrid board index)))

(defn all-sections [board]
  (concat
    (all-rows board)
    (all-columns board)
    (all-subgrids board)))

(defn is-valid-section [section]
  (let [known-cells (filter known-cell? section)
        known-vals (set (map :value known-cells))]
    (= (count known-cells) (count known-vals))))

(defn is-valid-board? [board]
  (every?
    is-valid-section
    (all-sections board)))

(defn check-validity [board]
  (if (not (is-valid-board? board))
    (prn "Board is not valid"))
  board)

(defn enhance-board [board]
  (-> board
    remove-guesses
    (iterate-until-no-change enhance-board-step)
    check-validity))

(defn accept-input [board row col input-str]
  (enhance-board
    (assoc-in board
      [row col]
      {:type :input
       :value input-str})))

(defn clear-cell [board row col]
  (enhance-board (assoc-in board [row col] init-cell)))

