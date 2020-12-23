(ns kakuro.solution
  (:require
   [kakuro.puzzle :as pu]
;;   [kakuro.dbgpuzzle :as dpu]
  ))

(defn collect-solution
  "if the grid is not empty, add it to the solutions"
  [solutions solution-grid]
  
  (if solution-grid
      (conj solutions solution-grid)
      solutions))

(defn save-puzzle-if
  "Returns collection containing the grid of puzzle, if it is a valid puzzle, solutions otherwise"
  [solutions puzzle]
  
  (if (pu/is-puzzle-solution? puzzle)

    (collect-solution solutions (:grid puzzle)) 

    solutions))

