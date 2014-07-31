(ns sudoku-helper.core
  (:gen-class)
  (:require [sudoku-helper.board.board :refer :all]))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (print-board init-board))

(-main)