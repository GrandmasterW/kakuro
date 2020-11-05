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
  (dotimes [n (count puzzles)]
    (do-puzzle (nth puzzles n)))  )
