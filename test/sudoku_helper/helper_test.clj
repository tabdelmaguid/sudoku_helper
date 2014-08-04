(ns sudoku-helper.helper-test
  (:require [clojure.test :refer :all]
            [sudoku-helper.helper :refer :all]))

(load "sample_boards")

(deftest test-set-column
  (testing "Set column should modify board"
    (let [new-col (vec (repeat 9 init-cell))
          old-board (to-cell-board board2)
          new-board (set-col old-board new-col 0)]
      (is (= new-col
             (get-column new-board 0))))))

(deftest test-guess-single-show-on-column
  (testing "Guess single show on columns correctly"
    (let [cell-board (to-cell-board board3)
          removed-knowns (remove-known-digits cell-board)
          guessed-board  (guess-single-show-on-col removed-knowns 0)]
      (is (= {:type :guess
              :value 6}
             (get-in guessed-board [7 0]))))))

(deftest test-guess-value
  (testing "If a values shows in one cell in a row, it changes to a guess on that row"
    (let [row (cons
                {:type :possibilities
                 :value #{1 2 3 4 9}}
                (repeat 8 {:type :possibilities
                           :value #{1 2 3 4 5 6 7 8}}))
          guessed-row (guess-single-show-on-section (vec row))]
      (is (= {:type :guess
              :value 9}
             (first guessed-row)))
      (is (= (second row)
             (second guessed-row)))))
  (testing "If a value shows only in one cell on a section, it changes to a guess on that cell"
    (let [cell-board (to-cell-board board2)
          remove-knows (remove-known-digits cell-board)
          board (guess-single-show remove-knows)]
      (is (=
            {:type :guess
             :value 8}
            (get-in board [1 7]))))))
