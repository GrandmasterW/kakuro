(ns kakuro.segment-test
  (:require [clojure.test :refer :all]
            [kakuro.point :as pt]
            [kakuro.segment :as seg]
            [kakuro.util :as util]
            ))

(deftest column-segment-test
  (testing "checks if column segment is created accordingly"
    (let [fy 2
          ty 4
          x 1
          sum 19
          cs (seg/column-segment fy ty x sum)]
      (is (and (= (:orientation cs) :v)
               (= (:b cs) x)
               (= (:from-a cs) fy)
               (= (:to-a cs) ty))))))

(deftest row-segment-test
  (testing "checks if row segment is created accordingly"
    (let [fx 2, tx 4, y 1, sum 19
          rs (seg/row-segment fx tx y sum)]
      (is (and (= (:orientation rs) :h)
               (= (:b rs) y)
               (= (:from-a rs) fx)
               (= (:to-a rs) tx))))))

(deftest create-points-from-segment-test
  (testing "check row segment"
    (let [fx 2, tx 7, y 2, s 19
          rowseg (seg/row-segment fx tx y s)
          points (seg/create-points-from-segment rowseg)]
      (is (and
           (= (count points) (inc (- tx fx)))
           (every? #(= (:y %1) y) points)
           (every? (partial util/betweenr fx tx) (map :x points))
           ))))
   (testing "check column segment"
    (let [fy 2, ty 7, x 2, s 21
          colseg (seg/column-segment fy ty x s)
          points (seg/create-points-from-segment colseg)]
      (is (and
           (= (count points) (inc (- ty fy)))
           (every? #(= (:x %1) x) points)
           ;;           (every? (partial betweenr fy ty) (map :y points))
           ;; TODO 
           ))))
  )

(deftest contains-point?-test
  (testing "horizontal: point 2/3 should be on segment [:h 1 4 3 19], but not on [:h 4 7 3 19] or on  [:h 1 7 2 19]"
    (let [xp 2, yp 3, p (pt/->Point xp yp)
          s1 (seg/row-segment 1 4 3 19)
          s2 (seg/row-segment 4 7 3 19)
          s3 (seg/row-segment 1 7 2 19)    ]
      (is (seg/contains-point? p s1))
      (is (not (seg/contains-point? p s2)))
      (is (not (seg/contains-point? p s3))))))
