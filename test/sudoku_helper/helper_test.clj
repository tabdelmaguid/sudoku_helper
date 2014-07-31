(ns sudoku-helper.helper-test
  (:require [clojure.test :refer :all]
            [sudoku-helper.helper :refer :all]))

(load "sample_boards")

(deftest test-guess-value
  (testing "If value shows only in one cell on a section, it changes to a guess on that cell"
    (let [cell-board (to-cell-board board2)
          board (guess-single-show cell-board)]
      (is (=
            {:type :guess
             :value 8}
            (get-in board [1 7]))))))
