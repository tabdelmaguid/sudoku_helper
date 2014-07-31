(ns sudoku-helper.core
  (:gen-class)
  (:require [sudoku-helper.board :refer :all]))

(defn -main
  [& args]
  (display-board))

(-main)