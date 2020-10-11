(ns kakuro.util-test
  (:require [clojure.test :refer :all]
            [kakuro.util :refer :all]))

(deftest between-test
  (testing "2 is between 1 and 7"
    (is (betweenl 2 1 7)))
  (testing "1 is between 1 and 7"
    (is (betweenl 1 1 7)))
  (testing "8 is not between 1 and 7"
    (is (not (betweenl 8 1 7)))))
