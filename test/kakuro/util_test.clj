(ns kakuro.util-test
  (:require [clojure.test :refer :all]
            [kakuro.util :refer :all]
            [clojure.set :as cs]))

(deftest betweenl-test
  (testing "2 is between 1 and 7"
    (is (betweenl 2 1 7)))
  (testing "1 is between 1 and 7"
    (is (betweenl 1 1 7)))
  (testing "8 is not between 1 and 7"
    (is (not (betweenl 8 1 7)))))

(deftest betweenr-test
  (testing "2 is betweenr 1 and 7"
    (is (betweenr 1 7 2)))
  (testing "1 is betweenr 1 and 7"
    (is (betweenr 1 7 1)))
  (testing "8 is not betweenr 1 and 7"
    (is (not (betweenr 1 7 8)))))

(deftest fullrange-test
  (testing "fullrange 0 to 10 contains 11 numbers, the difference of according sets is 0"
    (let [fr  (fullrange 0 10)
          frs (into #{} fr)
          ra  (range 0 11)
          ras (into #{} ra)
          ]
      (is (= (count fr) (count ra)))
      (is (empty? (cs/difference frs ras)))))
    (testing "fullrange 2 to 10 step 2 contains 5 numbers, the difference of according sets is 0"
    (let [fr  (fullrange 2 10 2)
          frs (into #{} fr)
          ra  (filter even? (range 1 11))
          ras (into #{} ra)
          ]
      (is (= (count fr) (count ra)))
      (is (empty? (cs/difference frs ras)))))
    (testing "fullrange 10 to 2 step -2 contains 5 numbers, the difference of according sets is 0"
    (let [fr  (fullrange 10 2 -2)
          frs (into #{} fr)
          ra  (filter even? (range 10 1 -2))
          ras (into #{} ra)
          ]
      (is (= (count fr) (count ra)))
      (is (empty? (cs/difference frs ras)))))
  )
  
