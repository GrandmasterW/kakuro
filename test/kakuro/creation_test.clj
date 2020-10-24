(ns kakuro.creation-test
  (:require [clojure.test :refer :all]
            [kakuro.creation :as cr]
            [kakuro.segment :as seg]
            [kakuro.point :as pt]
            [clojure.set :as cs]
            ))

(deftest create-points-for-segment-test
  (testing "simple: two points on segment :h, 3-4, 2"
    (let [fx 3, tx 4, y 2
          p1 (pt/->Point fx y), p2 (pt/->Point tx y),
          pts #{p1 p2}
          rs (seg/create-row-segment fx tx y 11 nil)
          rpts (cr/create-points-for-segment rs) ]
      (is (= (count rpts) (count pts)))
      (is (empty? (cs/difference pts (into #{} rpts)))))))

(deftest create-all-points-test
  (testing "creating from three segments"
    (let [x 3, y 4
          fx 1, tx (inc x)
          fy 1, ty (inc y)
          rs (seg/create-row-segment fx tx y 22 nil)
          cs (seg/create-column-segment fy ty x 24 nil)
          all-pts (cr/create-all-points [rs cs]) ; get all points
          ]
      (is (= (count all-pts)
             (dec (+ (- (inc tx) fx)
                     (- (inc ty) fy))))))))

(deftest assign-points-to-segment-test
  (testing "simple: all points on the h-segment must be in points at the end"
    (let [fx 1, tx 4, y 1, sr 17
          fy 1, ty 2, x 1, sc 19
          rs1 (seg/create-row-segment fx tx y sr nil)
          cs1 (seg/create-column-segment fy ty x sc nil)
          pts (cr/create-all-points [rs1 cs1])
          rs2 (cr/assign-points-to-segment pts rs1)
          ]
      (is (= (count (:points rs2)) (- tx fx -1)))
      (is (empty? (cs/difference (:points rs2) pts))))))
