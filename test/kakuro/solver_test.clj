(ns kakuro.solver-test
  (:require
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
;;   [kakuro.grid :as grid]
;;   [kakuro.util :as util]
   [kakuro.solver :as sol]
   [kakuro.importer :as imp]
;;   [clojure.set :as cs]
   [clojure.test :refer :all]
   ))


(deftest start-solve-test
  (testing "1x1 puzzle with max value 1"
    (let[puzzle (cr/create-puzzle [(seg/new-row 1 1 1 1)
                                      (seg/new-column 1 1 1 1)])
         des-result [{(pt/Point 1 1) #{1}}]]
         (is (= (sol/start-solve puzzle false) des-result))))
  (testing "1x1 puzzle with max value 2"
    (let[puzzle (cr/create-puzzle [(seg/new-row 1 1 1 2)
                                   (seg/new-column 1 1 1 2)])
         des-result [{(pt/Point 1 1) #{2}}]]
      (is (= (sol/start-solve puzzle false) des-result))))
  (testing "2x1 puzzle with max value 3 in row, 3 segments"
    (let[puzzle (cr/create-puzzle [(seg/new-row    1 2 1 3)
                                   (seg/new-column 1 1 1 1)
                                   (seg/new-column 1 1 2 2)
                                   ])
         des-result [{(pt/Point 1 1) #{1}
                      (pt/Point 2 1) #{2}}]
         ]
      (is (= (sol/start-solve puzzle false) des-result)))))


(def testfiles
  [
   "data/pi55hard.edn"
   "data/p66med.edn"
   "data/p44.edn"
   "data/p55easy.edn"
   "data/p66easy.edn"
   "data/pi77easy.edn"
   "data/pi88master.edn"
   "data/pw1010.edn"
   ])


(defn solved?
  "True, if all solutions in file match computed solutions" 
  [filename]
  (let [
        puzzle (imp/read-puzzle filename)
        p-solutions (into {} (:solutions puzzle))

        solutions (sol/start-solve puzzle false)
        sol-set (into {} solutions)
        ]
    (= p-solutions sol-set)))
  
(deftest start-solve-file-test
  (testing "solving puzzles from all files in list"
    (is (every? solved? testfiles))))

  
