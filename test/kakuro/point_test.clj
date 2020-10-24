(ns kakuro.point-test
  (:require [kakuro.point :refer :all]
            [clojure.test :refer :all]))

(deftest point-test
  (testing "testing if point contains given x and y"
    (let [p (->Point 2 3)]
      (is (and (= 2 (:x p))
               (= 3 (:y p))
               (= 2 (count (keys p)))))))
  (testing "testing high values for x and y"
    (let [x 100045
          y 100030
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

(deftest point-matches?-test
  (testing "check if :x of point 3 4 is 3 but not x=42 and y is 4"
    (let [x 3
          y 4
          p (->Point x y)]
      (is (point-matches? :x x p))
      (is (point-matches? :y y p))
      (is (not (point-matches? :x 42 p))))))

(deftest point-between?-test
  (testing "check if point 5/7 is in range 3 5 for x and 1 8 for y, but not in 1 4 for y"
    (let [x 5, y 7, p (->Point x y)
          xra [3 5]
          yra [1 8]
          ynra [1 4]]
      (is (point-between? p :x (first xra) (second xra) y))
      (is (point-between? p :y (first yra) (second yra) x))
      (is (not (point-between? p :y (first ynra) (second ynra) x))))))
      
    

