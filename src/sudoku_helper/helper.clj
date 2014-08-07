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

(defn cell-from-possibilities [possibilities]
  (cond
    (= 0 (count possibilities))
      (guess-cell-of -1)
    (= 1 (count possibilities))
      (guess-cell-of (first possibilities))
    :else
      (available-cell-of possibilities)))

(defn reduce-possibilties [board row-index column-index]
  (if (known-cell? (get-in board [row-index column-index]))
    board
    (let [determined-vals (collect-determined-vals-around board row-index column-index)
          remaining-digits (set/difference all-possibilities determined-vals)
          new-cell (cell-from-possibilities remaining-digits)]
      (assoc-in board [row-index column-index] new-cell))))

(def all-cell-indexes
  (for [row cell-range
        column cell-range]
    [row column]))

(defn remove-known-digits-step [board]
  (reduce
    (fn [board [row column]]
      (reduce-possibilties board row column))
    board
    all-cell-indexes))

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


(defn get-column [board column-index]
  (vec (nth (apply map vector board) column-index)))

(defn set-column [board col-index column]
  (reduce
    (fn [board row-index]
      (assoc-in board [row-index col-index] (column row-index)))
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

(defn set-subgrid [board index subgrid]
  (reduce
    (fn [board cell-index]
      (let [row (+ (* 3 (quot index 3)) (quot cell-index 3))
            col (+ (* 3 (mod index 3)) (mod cell-index 3))]
        (assoc-in board [row col] (subgrid cell-index))))
    board
    cell-range))

(defn get-row [board index] (board index))

(defn set-row [board index row]
  (assoc board index row))

(def section-types
  {:row     [get-row set-row]
   :column  [get-column set-column]
   :subgrid [get-subgrid set-subgrid]})

(defn guess-single-show-on-section-and-reduce [board index section-type]
  (let [[getter setter] (get section-types section-type)
        section (getter board index)
        guessed-section (guess-single-show-on-section section)]
    (if (= section guessed-section)
      board
      (-> board
        (setter index guessed-section)
        remove-known-digits))))

(defn guess-single-show-on-section-type [board section-type]
  (reduce
    (fn [board index]
        (guess-single-show-on-section-and-reduce board index section-type))
    board
    cell-range))

(defn guess-single-show-on-rows [board]
  (guess-single-show-on-section-type board :row))

(defn guess-single-show-on-columns [board]
  (guess-single-show-on-section-type board :column))

(defn guess-single-show-on-subgrids [board]
  (guess-single-show-on-section-type board :subgrid))

(defn guess-single-show [board]
  (-> board
    remove-known-digits
    guess-single-show-on-rows
    guess-single-show-on-columns
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

(defn clear-cell [board row col]
  (enhance-board (assoc-in board [row col] init-cell)))

