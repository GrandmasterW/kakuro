(ns kakuro.puzzle-test
  (:require [kakuro.puzzle :as pu]
            [kakuro.point :as pt]
            [kakuro.segment :as seg]
            [kakuro.util :as ut]
            [kakuro.creation :as cr]
;;            [kakuro.grid :as grid]
            [clojure.test :refer :all]))


(comment

  
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

(deftest is-open-puzzle?-test
  (let [segs [(seg/create-row-segment 1 2 1 3 nil)
              (seg/create-row-segment 1 2 2 4 nil)
              (seg/create-column-segment 1 2 1 4 nil)
              (seg/create-column-segment 1 2 2 3 nil)]
        puzzle (cr/create-puzzle segs) ]
    (testing "fully open / created puzzle"
      (is (pu/is-open-puzzle? puzzle)))
    (testing "fully set puzzle"
      (let [testpuz (-> puzzle
                        (assoc-in [:grid (pt/Point 1 1)] #{1})
                        (assoc-in [:grid (pt/Point 2 1)] #{2})
                        (assoc-in [:grid (pt/Point 1 2)] #{3})
                        (assoc-in [:grid (pt/Point 2 2)] #{1}))]
        (is (not (pu/is-open-puzzle? testpuz)))))
    ))

(deftest is-puzzle-valid?-test
  (let [segs [(seg/create-row-segment 1 2 1 3 nil)
              (seg/create-row-segment 1 2 2 4 nil)
              (seg/create-column-segment 1 2 1 4 nil)
              (seg/create-column-segment 1 2 2 3 nil)]
        puzzle (cr/create-puzzle segs) ]
    (testing "fully open / created puzzle is not valid"
      (is (not (pu/is-puzzle-valid? puzzle))))
    (testing "fully set puzzle with correct values"
      (let [testpuz (-> puzzle
                        (assoc-in [:grid (pt/Point 1 1)] #{1})
                        (assoc-in [:grid (pt/Point 2 1)] #{2})
                        (assoc-in [:grid (pt/Point 1 2)] #{3})
                        (assoc-in [:grid (pt/Point 2 2)] #{1}))]
        (is (pu/is-puzzle-valid? testpuz))))
    ))
)
