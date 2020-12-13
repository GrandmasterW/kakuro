(ns kakuro.solver
  (:require
   [kakuro.puzzle :as pu]
;   [kakuro.strpuzzle :as spz]
   [kakuro.util :as util]
   [kakuro.point :as pt]
   [kakuro.grid :as gr]
   [kakuro.logpuzzle :as lpu]
   ;; [kakuro.segment :as seg]
   [kakuro.restrict.core :as rst]
   ;; [clojure.set :as cs]
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

  (if (pu/is-puzzle-valid? puzzle)
      (collect-solution solutions (:grid puzzle)) ;; save it
    solutions))

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
                         (map #(solve-puzzle %1 [] trail))
                         (remove empty?))
        ]
    (if (seq v-solutions)
      (reduce concat solutions v-solutions)
      solutions)))

(defn solve-first-open
  "returns solutions from iterating at first open point.
   Requires a restricted puzzle and last solution coll."
  [puzzle solutions trail]

  (if-not (pu/is-correct-puzzle? puzzle)
    solutions
    ;; else
    (if-let [fop (pu/first-open-point puzzle)]
      ;; at least one open point?
      (iterate-values puzzle solutions fop (str trail "->" (pt/str-point fop)))
          
      ;; nothing open? Done!
      (save-puzzle-if solutions puzzle))))


(defn solve-puzzle
  "Returns solutions, expanded with current, restricted puzzle, if it is a solution. Otherwise we go down to iterating the values at the first open point."
  [puzzle solutions trail]
  {:pre [(not (util/count-steps!?))]}

;;  (lpu/log-puzzle "solve-puzzle" trail puzzle)
  
  (if-not (pu/is-correct-puzzle? puzzle)
    solutions ;; done for bad: do not proceed with broken puzzle
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


