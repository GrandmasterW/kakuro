(ns kakuro.puzzle-test
  (:require [kakuro.puzzle :as pu]
            [kakuro.point :as pt]
            [kakuro.segment :as seg]
            [kakuro.util :as ut]
            [kakuro.creation :as cr]
;;            [kakuro.grid :as grid]
            [clojure.test :refer :all]))

(deftest count-potential-solutions-test
  (testing "check the number of potential grids by multiplying the number of values"
    (let [fa 1, ta 3, fb 1, tb 3, v 7
          segments (into [] (concat
                            (mapv #(seg/create-row-segment fa ta %1 %2 nil) (ut/fullrange fb tb) (iterate inc v))
                            (mapv #(seg/create-column-segment fb tb %1 %2 nil)(ut/fullrange fa ta) (iterate inc v))))
          puzzle (cr/create-puzzle segments)
          ]
      (is (= (Math/pow 9 (count (keys (:grid puzzle))))
             (* 1.0 (pu/count-potential-solutions puzzle)))))))


