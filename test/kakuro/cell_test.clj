(ns kakuro.cell-test
  (:require [clojure.test :refer :all]
            [kakuro.point :as pt]
            [kakuro.cell :as ce]
            [kakuro.segment :as seg]
            [kakuro.util :as util]
            ))

(deftest cell-test
  (testing "testing if cell contains p with given x and y"
    (let [x 2
          y 3
          v (into [] (range 0 10))
          c (ce/->Cell (pt/->Point x y) v)]
      (is (and (= x (:x (:point c)))
               (= y (:y (:point c)))
               (= v (:values c))
               (= 2 (count (keys c)))))))
  )

(deftest create-cells-test
  (testing "cell creation for 2x2 grid"
    (let [min 1, max 4
          segments [(seg/row-segment 1 2 1 5)
                    (seg/row-segment 1 2 2 4)
                    (seg/column-segment 1 2 1 4)
                    (seg/column-segment 1 2 2 5)]
          n 4 ; total cell number 
          cells (ce/create-cells segments min max)]
      (is (and
           (= (count cells) n)
           (= (count (ce/cells-in-row cells 1)) 2)
           ))))
  (testing "cell creation for 2x2 grid"
    (let [min 1, max 7
          segments [(seg/row-segment 1 2 1 5)
                    (seg/row-segment 1 2 2 4)
                    (seg/column-segment 1 2 1 4)
                    (seg/column-segment 1 2 2 5)]
          n 4 ; total cell number 
          cells (ce/create-cells segments min max)
          pt (pt/->Point 2 2)
          ra1 (util/fullrange 1 4) 
          cell1 (first (filter #(= (:point %1) pt) cells))]
      (is (= (count cells) n))
      (is (= (count (ce/cells-in-row cells 1)) 2))
      (is (= ra1 (:values cell1))))))


(deftest cells-in-row-test
  (testing "finding cells in constructed rows"
    (let [row 2
          fx 1
          tx 8
          cells (ce/create-cells [(seg/row-segment fx tx row 19)] 1 9)]
    (is (= (count (ce/cells-in-row cells row)) (inc (- tx fx)))))))

(deftest cells-in-column-test 
  (testing "finding cells in constructed columns"
    (let [column 2
          fx 1
          tx 5
          fy 1
          ty 4
          yrange (range fy (inc ty))
          srange (range (+ fy 10) (+ ty 11))
          rows (mapv (partial seg/row-segment fx tx) yrange srange)
          cells (ce/create-cells rows 1 9)]
    (is (= (count (ce/cells-in-column cells column)) (- (inc ty) fy))))))

(deftest min-segment-sum-test
  (testing "min of column and row for 2/3 should be 4 as determined by column"
    (let [v 4 ;; what we are looking for
          s [(seg/row-segment 1 5 1 11)
             (seg/row-segment 1 5 2 12)
             (seg/row-segment 1 5 3 13)
             (seg/row-segment 1 5 4 14)
             ;; columns
             (seg/column-segment 1 4 1 21)
             (seg/column-segment 1 4 2 v)
             (seg/column-segment 1 4 3 23)]
          p (pt/->Point 2 3) ]
      (is (= (ce/min-segment-sum p s) v)))))

