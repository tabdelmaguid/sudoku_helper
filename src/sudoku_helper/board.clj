(ns sudoku-helper.board
  (:require [seesaw.core :as ss]
            [seesaw.border :as ssb]
            [sudoku-helper.helper :refer :all]))

(load "sample_boards")

(def init-board
  (nth
    (iterate
      (fn [c] (vec (repeat 9 c)))
      init-cell)
    2))

(def sudoku-board (atom init-board))

(defn re-init-board []
  (swap! sudoku-board (fn[_ b] (identity b)) init-board))

(defn cell-str [cell]
  (case (:type cell)
    :possibilities (str (:value cell))
    :input (str (:value cell))
    :guess (apply str [(:value cell) "'"])))

(defn format-possibilities [values]
  (map (partial apply str)
    (partition 3
      (for [i (range 1 10)]
        (if ((set values) i)
          (str " " i " ")
          "   ")))))

(defn cell-format [cell]
  (case (:type cell)
    :possibilities (format-possibilities (:value cell))
    :input ["         " (format "    %s    " (:value cell)) "         "]
    :guess ["         " (format "    %s'   " (:value cell)) "         "]))

(defn print-board [board]
  (let [row-sep (apply str (repeat 95 "="))
        cell-sep (apply str (repeat 95 "-"))]
    (println row-sep)
    (dotimes [row (count board)]
      (doseq [n (range 3)]
        (print "||")
        (doseq [subrow (partition 3 (nth board row))]
          (doseq [cell (butlast subrow)]
            (print (str (nth (cell-format cell) n) "|")))
          (print (str (nth (cell-format (last subrow)) n) "||")))
        (println))
      (if (zero? (mod (inc row) 3))
        (println row-sep)
        (println cell-sep)))))


(declare update-grid)

(defn reset-board-to [flat-board]
  (let [cell-board (to-cell-board flat-board)
        board (swap! sudoku-board (fn [_] (enhance-board cell-board)))]
    (update-grid board)))

(defn digit-selection[row col digit]
  (let [board (swap! sudoku-board accept-input row col digit)]
    (update-grid board)))

(defn digit-button [text row col]
  (let [button (ss/button :text text)]
    (when (integer? text)
      (ss/listen button
        :action (fn [e]
                  (digit-selection row col text))
        :key-typed (fn [e]
                         (prn e))))
    button))

(defn cell-button [cell row col]
  (let [button (ss/button :text (:value cell)
                          :border 5
                          :foreground (if (= (:type cell) :guess)
                                              :blue))]
    (ss/listen button
      :action (fn [e]
                (ss/alert (str "You clicked on " (:value cell) ", which is a " (:type cell)
                               ", at " [row col]))))
    button))

(defn cell-grid [cell row col]
  (ss/grid-panel
    :columns 3
    :border 5
    :items (for [i (range 1 10)]
             (if ((set (:value cell)) i)
               (digit-button i row col)
               (digit-button " " row col)))))

(defn cell-content[board row col]
  (let [cell (get-in board [row col])]
    (case (:type cell)
           :possibilities (cell-grid cell row col)
           :input (cell-button cell row col)
           :guess (cell-button cell row col))))

(defn subgrid-content [board sub-row sub-col]
  (for [i (range 3) j (range 3)]
    (let [cell-row (+ (* 3 sub-row) i)
          cell-col (+ (* 3 sub-col) j)]
      (cell-content board cell-row cell-col))))

(defn subgrid [board sub-row sub-col]
  (ss/grid-panel
    :columns 3
    :border (ssb/line-border)
    :items (subgrid-content board sub-row sub-col)))

(defn grid-content [board]
  (for [i (range 3) j (range 3)]
    (subgrid board i j)))

(def grid
  (ss/grid-panel
    :columns 3
    :items (grid-content @sudoku-board)))

(defn update-grid [board]
  (ss/config! grid :items (grid-content board)))

(def main-window (ss/frame :title "Sudoku Helper"
                           :content grid
                           :width 600
                           :height 600))

(defn display-board []
  (ss/native!)
  (-> main-window ss/show!))
