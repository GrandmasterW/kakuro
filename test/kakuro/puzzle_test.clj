(ns kakuro.puzzle-test
  (:require 
   [clojure.test :refer :all]
   [kakuro.puzzle :as pu]
   [kakuro.point :as pt]
   [kakuro.util :as ut]
   [kakuro.cell :as ce]
))

(deftest count-potential-grids-test
  (testing "check the number of potential grids by multiplying the number of values"
    (let [maxrange 6
          points [(pt/->Point 1 1) (pt/->Point 2 2) (pt/->Point 3 3)]
          vrange (into [] (ut/fullrange 1 maxrange))
          cells  (mapv #(ce/->Cell %1 vrange) points)]
      (is (= (apply * (take 3 (repeat maxrange)))
             (pu/count-potential-grids cells))))))


