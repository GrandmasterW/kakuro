(ns kakuro.solver-test
  (:require
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
   [kakuro.grid :as grid]
   [kakuro.util :as util]
   [kakuro.solver :as sol]
   [kakuro.puzzle :as puz]
   [clojure.set :as cs]
   [clojure.test :refer :all]
   ))


(deftest solve-test
  (testing "1x1 puzzle with max value 1"
    (let[puzzle (cr/create-puzzle [(seg/create-row-segment 1 1 1 1 nil)
                                      (seg/create-column-segment 1 1 1 1 nil)])
         des-result [{(pt/Point 1 1) #{1}}]]
         (is (= (sol/start-solve puzzle) des-result))))
  (testing "1x1 puzzle with max value 2"
    (let[puzzle (cr/create-puzzle [(seg/create-row-segment 1 1 1 2 nil)
                                   (seg/create-column-segment 1 1 1 2 nil)])
         des-result [{(pt/Point 1 1) #{2}}]]
      (is (= (sol/start-solve puzzle) des-result))))
  (testing "2x1 puzzle with max value 3 in row, 3 segments"
    (let[puzzle (cr/create-puzzle [(seg/create-row-segment    1 2 1 3 nil)
                                   (seg/create-column-segment 1 1 1 1 nil)
                                   (seg/create-column-segment 1 1 2 2 nil)
                                   ])
         des-result [{(pt/Point 1 1) #{1}
                      (pt/Point 2 1) #{2}}]
         ]
      (is (= (sol/start-solve puzzle) des-result)))))


