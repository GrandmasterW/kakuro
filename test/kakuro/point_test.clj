(ns kakuro.point-test
  (:require [clojure.test :refer :all]
            [kakuro.point :refer :all]))

(deftest point-test
  (testing "testing if point contains given x and y"
    (let [p (->Point 2 3)]
      (is (and (= 2 (:x p))
               (= 3 (:y p))
               (= 2 (count (keys p)))))))
  (testing "testing high values for x and y"
    (let [x 45
          y 30
          p (->Point x y)]
      (is (and (= x (:x p))
               (= y (:y p))
               (= 2 (count (keys p)))))))
  (testing "testing extreme values for x and y"
    (let [x -2000000
          y 30000000
          p (->Point x y)]
      (is (and (= x (:x p))
               (= y (:y p))
               (= 2 (count (keys p)))))))
  (testing "testing y is not given: error?"
    (let [x 9
          y nil
          p (->Point x y)]
      (is (and (= x (:x p))
               (nil? (:y p))
               (= 2 (count (keys p)))))))
  )
