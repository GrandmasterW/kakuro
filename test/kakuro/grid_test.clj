(ns kakuro.grid-test
  (:require
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
   [kakuro.grid :as grid]
   [kakuro.util :as util]
   [clojure.set :as cs]
   [clojure.test :refer :all]
   ))

(deftest make-empty-grid-test
  (testing "empty grid"
    (is (= (grid/make-empty-grid) {}))))

(deftest get-points-from-grid-test
  (testing "one segment with 5 points"
    (let [fx 1, tx 5, y 1 su 12, min 1, max 9
          s (seg/new-row fx tx y su)
          grid (cr/make-initial-grid-for-segments [s] min max)
          points (keys grid)]
      (is (= (count points) (- tx fx -1))
          (empty?
           (cs/difference
            (into #{} points)
            (into #{} (map #(pt/Point %1 y) (util/fullrange fx tx))))))))
    (testing "5 segment with 5 points"
    (let [fx 1, tx 5, y 1, ym 5, su 12, min 1, max 9
          hsegs (mapv #(seg/new-row fx tx %1 su) (util/fullrange y ym))
          grid (cr/make-initial-grid-for-segments hsegs min max)
          points (keys grid)]
      (is (= (count points) (* (- ym y -1)(- tx fx -1)))
          (empty?
           (cs/difference
            (into #{} points)
            (into #{} (map #(pt/Point %1 y) (util/fullrange fx tx)))))))))

(deftest value-string-test
  (testing "return meaningful value"
    (is (= "1" (grid/value-string #{1})))
    (is (= "!" (grid/value-string #{})))
    (is (= "(1 2 3 4 5 6 7 8 9)" (grid/value-string #{1 2 3 4 5 6 7 8 9})))))


(deftest is-open-point?-test
  (let [p1 (pt/Point 1 1)
        p2 (pt/Point 2 1)
;;        pts #{p1 p2}
;;        rs (seg/create-row-segment 1 2 1 3 pts)
        gr {p1 #{2}, p2 (into #{} (range 1 10))}
        gr2 (assoc gr p2 #{1})
        ]
  (testing "Point p2 is open"
    (is (grid/is-open-point? gr p2)))
  (testing "Point p1 is not open in gr"
    (is (not (grid/is-open-point? gr p1))))
  (testing "Point p2 is not open in gr2"
    (is (not (grid/is-open-point? gr2 p2))))))


(deftest open-grid-points-test
  (testing "empty"
    (is (= 1 0))))
            
(deftest fixed-grid-points-test
  (testing "empty"
    (is (= 1 0))))

(deftest segment-value-sum-test
  (testing "empty"
    (is (= 1 0))))

(deftest points-values-unique?-test
  (testing "empty"
    (is (= 1 0))))

(deftest all-segment-values-unique?-test
  (testing "empty"
    (is (= 1 0))))

(deftest fixed-segment-values-unique?-test
  (testing "empty"
    (is (= 1 0))))

(deftest is-correct-grid?-test
  (testing "empty"
    (is (= 1 0))))

                        
(comment




(deftest open-grid-points-test
  (let [p1 (pt/Point 1 1)
        p2 (pt/Point 2 1)
        pts #{p1 p2}
        rs (seg/create-row-segment 1 2 1 3 pts)
        gr {p1 #{2} p2 (into #{} (range 1 10))}
        gr2 (assoc gr p2 #{1})
        result1 (grid/open-grid-points gr)
        result2 (grid/open-grid-points gr2)
        ]
  (testing "find p2 as one and only"
    (is (and (= (count result1) 1)
             (= (first result1) p2))))
  (testing "find nothing in gr2"
    (is (empty? result2)))))

(deftest fixed-grid-points-test
  (let [p1 (pt/Point 1 1)
        p2 (pt/Point 2 1)
        pts #{p1 p2}
        rs (seg/create-row-segment 1 2 1 3 pts)
        gr {p1 #{2} p2 (into #{} (range 1 10))}
        gr2 (assoc gr p2 #{1})
        result1 (grid/fixed-grid-points gr)
        result2 (into #{} (grid/fixed-grid-points gr2))
        ]
  (testing "find p1 in grid 1"
    (is (= (count result1) 1))
    (is (= (first result1) p1)))
  (testing "find p1 and p2 in grid1"
    (is (= (count result2) 2))
    (is (every? (partial contains? result2) pts)))))

(deftest segment-value-sum-test
  (let [p1 (pt/Point 1 1)
        p2 (pt/Point 2 1)
        pts #{p1 p2}
        rs (seg/create-row-segment 1 2 1 3 pts)
        gr {p1 #{2} p2 (into #{} (range 1 10))}
        gr2 (assoc gr p2 #{1})
        ]
  (testing "sum of result1 should be 2"
    (is (= (grid/segment-value-sum gr rs) 2)))
  (testing "sum of result2 shoud be 3"
    (is (= (grid/segment-value-sum gr2 rs) 3)))))


(deftest segment-values-unique?-test
  (testing "really unique values"
    (let [rs (seg/create-row-segment 1 3 1 6 nil)
          puzzle (cr/create-puzzle [rs])
          finpuz (-> puzzle
                     (assoc-in [:grid (pt/Point 1 1)] #{1})
                     (assoc-in [:grid (pt/Point 2 1)] #{2})
                     (assoc-in [:grid (pt/Point 3 1)] #{3}))]
      (is (grid/segment-values-unique? (:grid finpuz) (first (:segments finpuz))))))
  (testing "not unique values"
    (let [rs (seg/create-row-segment 1 3 1 6 nil)
          puzzle (cr/create-puzzle [rs])
          finpuz (-> puzzle
                     (assoc-in [:grid (pt/Point 1 1)] #{1})
                     (assoc-in [:grid (pt/Point 2 1)] #{1}) ; the glitch
                     (assoc-in [:grid (pt/Point 3 1)] #{3}))]
      (is (not (grid/segment-values-unique?
                (:grid finpuz)
                (first (:segments finpuz)))))))
  )

)
