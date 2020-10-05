(ns kakuro.cell-test
  (:require [clojure.test :refer :all]
            [kakuro.point :refer :all]
            [kakuro.cell :refer :all]
            ))

(deftest cell-test
  (testing "testing if cell contains p with given x and y"
    (let [x 2
          y 3
          v (into [] (range 0 10))
          c (->Cell (->Point x y) v)]
      (is (and (= x (:x (:point c)))
               (= y (:y (:point c)))
               (= v (:values c))
               (= 2 (count (keys c)))))))
  )
