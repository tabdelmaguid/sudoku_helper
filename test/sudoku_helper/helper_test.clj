(ns sudoku-helper.helper-test
  (:require [clojure.test :refer :all]
            [sudoku-helper.helper :refer :all]))

(load "sample_boards")

(deftest test-set-column
  (testing "Set column should modify board"
    (let [new-col (vec (repeat 9 init-cell))
          old-board (to-cell-board board2)
          new-board (set-column old-board 0 new-col)]
      (is (= new-col
             (get-column new-board 0))))))

(deftest test-guess-single-show-on-column
  (testing "Guess single show on columns correctly"
    (let [cell-board (to-cell-board board3)
          removed-knowns (remove-known-digits cell-board)
          guessed-board  (guess-single-show-on-section-and-reduce removed-knowns 0 :column)]
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

(defn same-inputs? [& boards]
  (every?
    (fn [[row-i col-i]]
      (let [cell-types (map #(:type (get-in % [row-i col-i])) boards)]
        (if (some #{:input} cell-types)
          (apply = cell-types)
          true)))
    all-cell-indexes))

(deftest stability-test
  (testing "verify results of reductions are stable"
    (let [cell-board (to-cell-board board1)
          reduced-board (enhance-board cell-board)]
      (is (same-inputs? cell-board reduced-board)))))
