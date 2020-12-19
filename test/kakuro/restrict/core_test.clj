(ns kakuro.restrict.core-test
  (:require
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
   [kakuro.grid :as grid]
   [kakuro.util :as util]
   [kakuro.puzzle :as puz]
   [kakuro.restrict.core :as rc]
   [clojure.set :as cs]
   [clojure.test :refer :all]
   ))

(comment
  
(deftest restrict-segment-test
  (let [rs (seg/new-row 1 2 1 3)
        puzzle (cr/create-puzzle [rs])
        new-rs (first (:segments puzzle))
        new-grid (rst/restrict-segment puzzle new-rs)
        point-values (vals new-grid)
        res-values #{1 2}] ;;
  (testing "both points should have values 1 and 2 only"
    (is (every? #(= 2 (count %1)) point-values))
    (is (every? #(empty? (cs/difference res-values %1)) point-values)))))


(deftest restrict-values-test
  (testing "puzzle grid points will all have the res-values set"
    (let [rs (seg/new-row 1 2 1 3)
          puzzle (cr/create-puzzle [rs])
          res-values #{1 2}] ;;
      (is (every?
           #(empty? (cs/difference res-values %1))
           (vals (:grid puzzle))))))
  (testing "2x2 Puzzle grid points" 
    (let [segs [(seg/new-row 1 2 1 3)
                (seg/new-row 1 2 2 4)
                (seg/new-column 1 2 1 4)
                (seg/new-column 1 2 2 3)]
          puzzle (cr/create-puzzle segs)
          values1 {(pt/Point 1 1) #{1 3}
                   (pt/Point 2 1) #{1 2}
                   (pt/Point 1 2) #{1 3}
                   (pt/Point 2 2) #{1 2}}
          res-puzzle (rst/restrict-values puzzle)
          ] ;;
      (is (= (:grid res-puzzle) values1)))))

)
