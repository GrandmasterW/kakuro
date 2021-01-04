(ns kakuro.puzzle-test
  (:require [kakuro.puzzle :as pu]
            [kakuro.point :as pt]
            [kakuro.segment :as seg]
            [kakuro.util :as ut]
            [kakuro.importer :as imp]
            [kakuro.creation :as cr]
;;            [kakuro.grid :as grid]
            [clojure.test :refer :all]))




;; ----------------------------------------------------------------------
;; some puzzles to use in tests
;;
;; ----- 2x2 -----
(def segments-2x2
  [(seg/new-row 1 2 1 3 )
   (seg/new-row 1 2 2 4 )
   (seg/new-column 1 2 1 4 )
   (seg/new-column 1 2 2 3 )])
(def puzzle-2x2 (cr/create-puzzle segments-2x2) )


;; ----- 5x5 -----
(def segments-5x5
  (into [] (concat
            ;;         :rows
            (mapv (fn [[f-a t-a b s]] (seg/new-row f-a t-a b s))
                  [[1 2 1 14]
                   [1 3 2 11]
                   [2 4 3 20]
                   [3 5 4 13]
                   [4 5 5 3] ])
            ;; :columns
            (mapv (fn [[f-a t-a b s]] (seg/new-column f-a t-a b s))
                  [[1 2 1 17]
                   [1 3 2 12]
                   [2 4 3 13]
                   [3 5 4 16]
                   [4 5 5 3] ]     )    )))

(def puzzle-5x5 (cr/create-puzzle segments-5x5))

(def solution-5x5
  (imp/convert-solution-str 
   "9 5 - - -
  8 1 2 - -
  - 6 5 9 -
  - - 6 5 2
  - - - 2 1"
))

;; ----------------------------------------------------------------------

(deftest puzzle-dimensions-test
  (testing "creating a 3x4 puzzle should bring 3 and 4"
    (let [segs [(seg/new-row 1 3 1 10)
                (seg/new-row 1 3 2 12 )
                (seg/new-row 1 3 3 9 )
                (seg/new-row 1 3 4 11 )

                (seg/new-column 1 4 1 17 )
                (seg/new-column 1 4 2 15 )
                (seg/new-column 1 4 3 12 )
                ]
          puzzle (cr/create-puzzle segs) ]
    (is (= [3 4] (pu/puzzle-dimensions puzzle))))))


(deftest count-potential-solutions-test
  (testing "check the number of potential grids by multiplying the number of values"
    (let [fa 1, ta 3, fb 1, tb 3, v 7
          segments (into [] (concat
                            (mapv #(seg/new-row fa ta %1 %2)   (ut/fullrange fb tb) (iterate inc v))
                            (mapv #(seg/new-column fb tb %1 %2)(ut/fullrange fa ta) (iterate inc v))))
          puzzle (cr/create-puzzle segments)
          ]
      (is (= (Math/pow 9 (count (keys (:grid puzzle))))
             (* 1.0 (pu/count-potential-solutions puzzle)))))))

(deftest is-open-puzzle?-test
  (testing "2x2 fully open / created puzzle"
    (is (pu/is-open-puzzle? puzzle-2x2)))
  (testing "2x2 fully set puzzle"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 1)] #{1})
                      (assoc-in [:grid (pt/Point 2 1)] #{2})
                      (assoc-in [:grid (pt/Point 1 2)] #{3})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (not (pu/is-open-puzzle? testpuz)))))
  (testing "2x2 partially open puzzle"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 2)] #{3})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
        (is (pu/is-open-puzzle? testpuz))))
  )

(deftest is-puzzle-solution?-test
  (testing "fully open / created puzzle is not valid"
    (is (not (pu/is-puzzle-solution? puzzle-2x2))))
  (testing "fully set puzzle with correct values"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 1)] #{1})
                      (assoc-in [:grid (pt/Point 2 1)] #{2})
                      (assoc-in [:grid (pt/Point 1 2)] #{3})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (pu/is-puzzle-solution? testpuz))))
  (testing "partially set puzzle with correct values, but open is not a solution"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 2 1)] #{2})
                      (assoc-in [:grid (pt/Point 1 2)] #{3})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (not (pu/is-puzzle-solution? testpuz)))))
  (testing "fully set puzzle with open values -> no solution"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 1)] #{1})
                      (assoc-in [:grid (pt/Point 2 1)] #{2})
                      (assoc-in [:grid (pt/Point 1 2)] #{2})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (not (pu/is-puzzle-solution? testpuz)))))
  (testing "fully set puzzle with repeating values -> no solution"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 1)] #{1})
                      (assoc-in [:grid (pt/Point 2 1)] #{1})
                      (assoc-in [:grid (pt/Point 1 2)] #{3})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (not (pu/is-puzzle-solution? testpuz)))))
  )


(deftest is-correct-puzzle?-test
  (testing "fully open / created puzzle is correct"
    (is (pu/is-correct-puzzle? puzzle-2x2)))
  (testing "fully set puzzle with correct values"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 1)] #{1})
                      (assoc-in [:grid (pt/Point 2 1)] #{2})
                      (assoc-in [:grid (pt/Point 1 2)] #{3})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (pu/is-correct-puzzle? testpuz))))
  (testing "fully set puzzle with one empty value set: not ok"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 1)] #{1})
                      (assoc-in [:grid (pt/Point 2 1)] #{2})
                      (assoc-in [:grid (pt/Point 1 2)] #{})
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (not (pu/is-correct-puzzle? testpuz)))))
  (testing "fully set puzzle with one nil value set: not ok"
    (let [testpuz (-> puzzle-2x2
                      (assoc-in [:grid (pt/Point 1 1)] #{1})
                      (assoc-in [:grid (pt/Point 2 1)] #{2})
                      (assoc-in [:grid (pt/Point 1 2)] nil)
                      (assoc-in [:grid (pt/Point 2 2)] #{1}))]
      (is (not (pu/is-correct-puzzle? testpuz)))))
    )
    
(deftest distinct-fixed-values?-test
  (testing "none"
    (let [testpuzzle (assoc puzzle-5x5 :grid solution-5x5)]
      (is (pu/distinct-fixed-values? testpuzzle))))
    )

(deftest assert-points-match-test
  (testing "correct puzzle"
    (is (pu/assert-points-match puzzle-5x5)))
  (testing "broken in first segment"
    (let [
          segs-fail (assoc-in segments-5x5 [0 :to-a] 4) ; setting to-a in first segment
          puzzle-fail (cr/create-puzzle segs-fail)
          ]
      (is (try
            (pu/assert-points-match puzzle-fail)
            (catch AssertionError _ true)))))
  (testing "broken in last segment"
    (let [
          segs-fail (assoc-in segments-5x5 [9 :to-a] 8)
          puzzle-fail (cr/create-puzzle segs-fail)
          ]
      (is (try
            (pu/assert-points-match puzzle-fail)
            (catch AssertionError _ true)))))
  )

(deftest merge-puzzle-grid-test
  (testing "none"
    (is false)))

    
(deftest first-open-point-test
  (testing "none"
    (is false)))

(deftest all-segment-sums-done?-test
  (testing "none"
    (is false)))


