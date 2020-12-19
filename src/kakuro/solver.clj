(ns kakuro.solver
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.util :as util]
   [kakuro.point :as pt]
   [kakuro.grid :as gr]
   [kakuro.logpuzzle :as lpu]
   [kakuro.dbgpuzzle :as dpu]
   [kakuro.restrict.core :as rst]
   )
  )


(defn collect-solution
  "if the grid is not empty, add it to the solutions"
  [solutions solution-grid]
  
  (if solution-grid
      (conj solutions solution-grid)
      solutions))

(defn save-puzzle-if
  "Returns collection containing the grid of puzzle, if it is a valid puzzle, solutions otherwise"
  [solutions puzzle]
  (dpu/dbg-puzzle "save-puzzle-if#1" puzzle)
  (if (pu/is-puzzle-solution? puzzle)
    (do   (dpu/dbg-puzzle "save-puzzle-if#2" puzzle)
      (collect-solution solutions (:grid puzzle)) ;; save it
    )
    solutions))

(defn continue-solve?
  "true, if ... (and)
   puzzle is syntactically correct, no empty value sets
   puzzle is open, i.e. has open points
   fixed points in each segment have distinct values
   Hint: puzzle may have be a solution, we do not check this here  "
  [puzzle]
  (let [r  (and
            (seq puzzle)
            (pu/is-correct-puzzle? puzzle)
            (pu/is-open-puzzle? puzzle)
            (pu/distinct-fixed-values? puzzle)
            )]
    ;;(dpu/dbg-puzzle (str "r: " r) puzzle)
    r)
  )

(defn puzzle-variations 
  "Returns a vector of puzzles, each containing a different value at point as single value"
  [point puzzle]
  (let [values (get (:grid puzzle) point)]
    (filter (comp gr/is-correct-grid? :grid)
            (mapv #(assoc-in puzzle [:grid point] #{%1}) values))))


(declare solve-puzzle)

(defn iterate-values
  "Walks through the values at first-open-point, fixes them for the point and goes into solve again"
  [puzzle solutions first-open-point trail]

  (let [
        v-solutions (->> puzzle
                         (puzzle-variations first-open-point)
                         (mapv #(solve-puzzle %1 [] trail))
                         (remove empty?))
        ]
    (if (seq v-solutions)
      (reduce concat solutions v-solutions)
      solutions)))

(defn solve-first-open
  "returns solutions from iterating at first open point.
   Requires a restricted puzzle and last solution coll."
  [puzzle solutions trail]

;;  (if-not (continue-solve? puzzle)
;;    (save-puzzle-if solutions puzzle)
    ;; else
  (if-let [fop (pu/first-open-point puzzle)]
    ;; at least one open point?
    (iterate-values puzzle solutions fop (str trail "->" (pt/str-point fop)))
          
    ;; nothing open? Done!
    (save-puzzle-if solutions puzzle)))


(defn solve-puzzle
  "Returns solutions, expanded with current, restricted puzzle, if it is a solution. Otherwise we go down to iterating the values at the first open point."
  [puzzle solutions trail]
  {:pre [(not (util/count-steps!?))]}

  (lpu/log-puzzle "solve-puzzle" trail puzzle)
  
  (if-not (continue-solve? puzzle)
    (save-puzzle-if solutions puzzle)
    (solve-first-open
     (rst/restrict-puzzle puzzle)
     solutions
     trail)))
      

(defn start-solve
  "returns a collection of grids that are solutions for the puzzle, each having only one value for each grid cell, matching all criteria"
   [puzzle]
  (util/reset-steps!)
  (let [solutions (trampoline #(solve-puzzle puzzle [] ""))]
    (lpu/log-steps-solutions  (util/get-steps)
                              (count solutions))
    solutions))


