(ns kakuro.core
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.strpuzzle :as sp]
   [kakuro.util :as ut]
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
   [kakuro.solver :as sol]
   )
  (:gen-class))

(defn- row [fx tx y s]
  (seg/create-row-segment fx tx y s nil))

(defn- col [fy ty x s]
  (seg/create-column-segment fy ty x s nil))

(def pac
  (cr/create-puzzle
   [(row 4 7 2 19)
    (row 4 8 3 31)
    (row 2 3 4 5) (row 4 6 4 17) (row 8 9 4 5)
    (row 2 3 5 23)(row 6 7 5 15) (row 9 10 5 5)
    (row 2 10 6 45)
    (row 2 3 7 4) (row 5 6 7 4)  (row 8 10 7 7)
    (row 3 4 8 9) (row 6 7 8 3)  (row 9 10 8 4)
    (row 4 8 9 31)
    (row 5 8 10 17)
    ;; columns
    (col 4 7 2 12)
    (col 4 8 3 19)
    (col 2 3 4 17) (col 5 6 4 16) (col 8 9 4 16)
    (col 2 4 5 23) (col 6 7 5 3)  (col 9 10 5 16)
    (col 2 10 6 45)
    (col 2 3 7 4)  (col 5 6 7 17) (col 8 10 7 7)
    (col 3 4 8 7)  (col 6 7 8 5)  (col 9 10 8 4)
    (col 4 8 9 19)
    (col 5 8 10 14)
    ]))


;; real example from kakuro
(def puzzles
  [ ;; 0 
   ;;3x3-1
   (cr/create-puzzle
    [(seg/create-row-segment 2 3 1 16 nil)
     (seg/create-row-segment 1 3 2 15 nil)
     (seg/create-row-segment 1 3 3 19 nil)
     ;; columns
     (seg/create-column-segment 2 3 1 17 nil)
     (seg/create-column-segment 1 3 2 23 nil)
     (seg/create-column-segment 1 3 3 10 nil) ] )
   ;; 1
   ;;"2x2 max 7"
   (cr/create-puzzle
    [(seg/create-row-segment 1 2 1 3 nil)
     (seg/create-row-segment 1 2 2 7 nil)
     (seg/create-column-segment 1 2 1 4 nil)
     (seg/create-column-segment 1 2 2 6 nil)])
   ;; 2
   ;;"2x2 max 4"
   (cr/create-puzzle [(seg/create-row-segment    1 2 1 3 nil)
                      (seg/create-row-segment    1 2 2 4 nil)
                      (seg/create-column-segment 1 2 1 4 nil)
                      (seg/create-column-segment 1 2 2 3 nil)])
   ])

(defn do-puzzle [puzzle]
  (println "------------------ Start --------------------------------")
  (sp/print-puzzle-stats puzzle)
  (let [solutions (sol/start-solve puzzle)]
    (dotimes [n (count solutions)]
      (sp/print-puzzle (assoc puzzle :grid (nth solutions n)))
      (println "  ---  ")
      ))
  (println "-------------------End   -------------------------------")
  )

(defn -main
  "print"
  [& args]

  (do-puzzle pac))

  (comment
    (dotimes [n (count puzzles)]
    (do-puzzle (nth puzzles n)))  )
