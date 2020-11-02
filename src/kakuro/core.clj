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
(def puzzle1 (cr/create-puzzle
              [(seg/create-row-segment 2 3 1 16 nil)
               (seg/create-row-segment 1 3 2 15 nil)
               (seg/create-row-segment 1 3 3 19 nil)
               ;; columns
               (seg/create-column-segment 2 3 1 17 nil)
               (seg/create-column-segment 1 3 2 23 nil)
               (seg/create-column-segment 1 3 3 10 nil) ] ))

;;(def puzzle1 (assoc-in puzzle1 [:grid (pt/Point 1 2)] #{1}))

;; simple example
(def puzzle2 (cr/create-puzzle
              [(seg/create-row-segment 1 2 1 3 nil)
               (seg/create-row-segment 1 2 2 7 nil)
               (seg/create-column-segment 1 2 1 4 nil)
               (seg/create-column-segment 1 2 1 6 nil)]))

(def puzzle2x2
  (cr/create-puzzle [(seg/create-row-segment    1 2 1 3 nil)
                     (seg/create-row-segment    1 2 2 4 nil)
                     (seg/create-column-segment 1 2 1 4 nil)
                     (seg/create-column-segment 1 2 2 3 nil)]))

(defn do-puzzle [puzzle]
  (println "--------------------------------------------------")
  (sp/print-puzzle-stats puzzle)
  (println "--------------------------------------------------")
  (let [solutions (sol/start-solve puzzle)]
    (println solutions)) ;; this is a list of grids! 
  (println "--------------------------------------------------")
  )

(defn -main
  "print"
  [& args]
  (do-puzzle puzzle2)
  (do-puzzle puzzle2x2)
  (do-puzzle puzzle1)
  )
