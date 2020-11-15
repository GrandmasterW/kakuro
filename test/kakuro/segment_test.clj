(ns kakuro.segment-test
  (:require [kakuro.segment :as seg]
            [clojure.test :refer :all]
            [kakuro.point :as pt]
            [kakuro.creation :as cr]
            [clojure.set :as cs]))

(deftest new-column-segment-test
  (testing "checks if column segment is created accordingly"
    (let [fy 2, ty 4, x 1, sum 19
          cs (seg/new-column fy ty x sum)]
      (is (= (:orientation cs) :v))
      (is (= (:b cs) x))
      (is (= (:from-a cs) fy))
      (is (= (:to-a cs) ty)))))

(deftest new-row-test
  (testing "checks if row segment is created accordingly"
    (let [fx 2, tx 4, y 1, sum 19
          rs (seg/new-row fx tx y sum)]
      (is (= (:orientation rs) :h))
      (is (= (:b rs) y))
      (is (= (:from-a rs) fx))
      (is (= (:to-a rs) tx)))))

(deftest is-row-segment?-test
  (testing "positive test for row, negative for column"
    (let [fx 2, tx 4, y 1, sum 19
          rs (seg/new-row fx tx y sum)
          cs (seg/new-column fx tx y sum)]
      (is (seg/is-row-segment? rs))
      (is (not (seg/is-row-segment? cs))))))

(deftest is-column-segment?-test
  (testing "positive test for column, negative for row"
    (let [fx 2, tx 4, y 1, sum 19
          rs (seg/new-row fx tx y sum)
          cs (seg/new-column fx tx y sum)]
      (is (not (seg/is-column-segment? rs)))
      (is (seg/is-column-segment? cs)))))

(deftest contains-point-in-set?-test
  (testing "positive test  3/4 on 1-3/1-5"
    (let [x 3, y 4, p (pt/Point x y)
          pts #{p}
          fx 1, tx x
          fy 1, ty (inc y)
          rs (seg/create-row-segment fx tx y 22 pts)
          cs (seg/create-column-segment fy ty x 24 pts)          ]
      (is (seg/contains-point-in-set? p rs))
      (is (seg/contains-point-in-set? p cs))))
  (testing "test 3/4 on 1-3/1-5 and negative test outside limits"
    (let [x 3, y 4, p (pt/Point x y)
          pts #{ (pt/Point 7 8)}
          fx 1, tx x
          fy 1, ty (inc y)
          rs (seg/create-row-segment fx tx y 22 pts)
          cs (seg/create-column-segment fy ty x 24 pts)          ]
      (is (not (seg/contains-point-in-set? p rs)))
      (is (not (seg/contains-point-in-set? p cs))))))

(deftest segments-for-point-test
  (testing "find two segments, one horizontal, one vertical"
    (let [x 3, y 4, p (pt/Point x y)
          fx 1, tx 4
          fy 1, ty 5
          rs (seg/new-row fx tx y 22)
          cs (seg/new-column fy ty x 24)
          s1 [rs cs]
          all-pts (cr/create-all-points s1) 
          segments (mapv (partial cr/assign-points-to-segment all-pts) s1)
          res (seg/segments-for-point p segments)
          ]
      (is (= (count res) 2))
      (is (= (count all-pts)
             (dec (reduce + (map (comp count :points) segments)))))))
   (testing "find two segments, one horizontal, one vertical, but not 3"
    (let [x 3, y 4, p (pt/Point x y)
          fx 1, tx 4
          fy 1, ty 5
          rs (seg/new-row fx tx y 22)
          cs (seg/new-column fy ty x 24)
          ncs (seg/new-column (* 2 y)(* 3 y) x 3000) ; no match
          all-pts (cr/create-all-points [rs cs ncs]) ; get all points
          segments (mapv (partial cr/assign-points-to-segment all-pts) [rs cs ncs])
          res (seg/segments-for-point p segments)
          ]
      (is (= (count res) 2))
      (is (= (count all-pts)
             (dec (reduce + (map (comp count :points) segments)))))))
  (testing "find no segment"
    (let [x 3, y 4,
          p (pt/Point (* 4 x)(* 4 y))
          fx 1, tx (inc x)
          fy 1, ty (inc y)
          rs (seg/new-row fx tx y 22)
          cs (seg/new-column fy ty x 24)
          ncs (seg/new-column (* 2 y)(* 3 y) x 3000) ; no match
          all-pts (cr/create-all-points [rs cs ncs]) ; get all points
          segments (mapv (partial cr/assign-points-to-segment all-pts) [rs cs ncs])
          res (seg/segments-for-point p segments)
          ]
      (is (= (count res) 0)))))


