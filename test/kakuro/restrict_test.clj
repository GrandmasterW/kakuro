(ns kakuro.restrict-test
  (:require
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
   [kakuro.grid :as grid]
   [kakuro.util :as util]
   [kakuro.puzzle :as puz]
   [kakuro.restrict :as rst]
   [clojure.set :as cs]
   [clojure.test :refer :all]
   ))

(deftest restrict-segment-test
  (let [rs (seg/create-row-segment 1 2 1 3 nil)
        puzzle (cr/create-puzzle [rs])
        new-rs (first (:segments puzzle))
        new-grid (rst/restrict-segment puzzle new-rs)
        point-values (vals new-grid)
        res-values #{1 2}] ;;
  (testing "both points should have values 1 and 2 only"
    (is (every? #(= 2 (count %1)) point-values))
    (is (every? #(empty? (cs/difference res-values %1)) point-values)))))

(deftest reduce-grid-point-test
  (testing "reducing to a new value list from point"
    (let [pt (pt/->Point 2 1)
          pt-vals #{1 2}
          grid {pt #{1 2 3}} ]
      (is (= {pt pt-vals} (rst/reduce-grid-point grid [pt pt-vals]))))))

(deftest reduce-grid-part-test
  (testing "reducing to a new value list from point"
    (let [pt1 (pt/->Point 1 1), pt2 (pt/->Point 2 1)
          pt-vals1 #{1 2 3 4}, pt-vals2 #{1 2}
          grid {pt1 #{1 2}, pt2 #{1 2 3}}
          grid-part {pt1 pt-vals1, pt2 pt-vals2}]
      (is (= {pt1 #{1 2}, pt2 #{1 2}} (rst/reduce-grid-part grid
      grid-part))))))

(deftest restrict-values-test
  (testing "puzzle grid points will all have the res-values set"
    (let [rs (seg/create-row-segment 1 2 1 3 nil)
          puzzle (cr/create-puzzle [rs])
          res-values #{1 2}] ;;
      (is (every?
           #(empty? (cs/difference res-values %1))
           (vals (:grid puzzle))))))
  (testing "2x2 Puzzle grid points" 
    (let [segs [(seg/create-row-segment 1 2 1 3 nil)
                (seg/create-row-segment 1 2 2 4 nil)
                (seg/create-column-segment 1 2 1 4 nil)
                (seg/create-column-segment 1 2 2 3 nil)]
          puzzle (cr/create-puzzle segs)
          values1 {(pt/->Point 1 1) #{1 2}
                   (pt/->Point 2 1) #{1 2}
                   (pt/->Point 1 2) #{1 2 3}
                   (pt/->Point 2 2) #{1 2}}
          res-puzzle (rst/restrict-values puzzle)
          ] ;;
      (is (= (:grid res-puzzle) values1)))))

