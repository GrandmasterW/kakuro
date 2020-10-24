(ns kakuro.grid-test
  (:require
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
   [kakuro.grid :as grid]
   [kakuro.util :as util]
   [clojure.set :as cs]
   [clojure.test :refer :all]
   ))


(deftest get-points-from-grid-test
  (testing "retrieve what we've stored"
    (let [fx 1, tx 5, y 1 su 12, min 1, max 9
          s (seg/create-row-segment fx tx y su nil)
          grid (cr/make-initial-grid-for-segments [s] min max)
          points (keys grid)]
      (is (= (count points) (- tx fx -1))
          (= (empty?
              (cs/difference
               (into #{} points)
               (into #{} (map #(pt/->Point %1 y) (util/fullrange fx tx))))))))))


(deftest value-string-test
  (testing "return meaningful value"
    (is (= "1" (grid/value-string #{1})))
    (is (= "?" (grid/value-string #{1 2 3 4 5 6 7 8 9})))))

(deftest is-open-point?-test
  (let [p1 (pt/->Point 1 1)
        p2 (pt/->Point 2 1)
        pts #{p1 p2}
        rs (seg/create-row-segment 1 2 1 3 pts)
        gr {p1 #{2} p2 (into #{} (range 1 10))}
        gr2 (assoc gr p2 #{1})
        ]
  (testing "Point p2 is open"
    (is (grid/is-open-point? gr p2)))
  (testing "Point p1 is not open in gr"
    (is (not (grid/is-open-point? gr p1))))
  (testing "Point p2 is not open in gr2"
    (is (not (grid/is-open-point? gr2 p2))))))

(deftest open-grid-points-test
  (testing ""
    (is false)))

(deftest fixed-grid-points-test
  (testing ""
    (is false)))

(deftest segment-value-sum-test
  (testing ""
    (is false)))

