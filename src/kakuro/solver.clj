(ns kakuro.solver
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.strpuzzle :as spz]
   [kakuro.util :as util]
   [kakuro.point :as pt]
   [kakuro.grid :as gr]
   [kakuro.creation :as cr]
   [kakuro.segment :as seg]
   [kakuro.restrict :as rst]
   [clojure.set :as cs]
   )
  )


(defn collect-solution [solutions solution-grid]
  "if the grid is not empty, add it to the solutions"
  (if solution-grid
    (do
      (conj solutions solution-grid))
      solutions))

(defn save-puzzle-if [solutions puzzle]
  "Returns collection containing the grid of puzzle, if it is a valid puzzle, solutions otherwise"
  (if (pu/is-puzzle-valid? puzzle) 
    (collect-solution solutions (:grid puzzle)) ;; save it 
    solutions))

(defn puzzle-variations [puzzle point trail]
  "Returns a vector of puzzles, each containing a different value at point as single value"
  (let [values (get (:grid puzzle) point)]
    (filter (comp gr/is-correct-grid? :grid)
            (mapv #(assoc-in puzzle [:grid point] #{%1}) values))))

(declare solve-puzzle)

(defn iterate-values [puzzle solutions first-open-point trail]
  "Walks through the values at first-open-point, fixes them for the point and goes into solve again"

;;  (util/log "iterate-values" "trail:" trail first-open-point (get (:grid puzzle) first-open-point))

  (let [c-puzzles (puzzle-variations puzzle first-open-point trail)
        v-solutions (remove empty?
                            (map
                             #(solve-puzzle %1
                                            []
                                            (str trail "->" (pt/str-point %1)))
                             c-puzzles))   ]
    (if (seq v-solutions)
      (reduce concat solutions v-solutions)
      solutions)))

(defn solve-puzzle [puzzle solutions trail]
  {:pre [(not (util/count-steps!?))]}
  "Returns solutions, expanded with current, restricted puzzle, if it is a solution. Otherwise we go down to iterating the values at the first open point."
;;  (util/log "solve-puzzle" "trail:" trail)
  ;; (println " --- ")
  ;;  (spz/print-puzzle puzzle)
  (if (not (gr/is-correct-grid? (:grid puzzle))) solutions
    (let [r-puzzle (rst/restrict-values puzzle)]
      (if (not (gr/is-correct-grid? (:grid r-puzzle))) solutions
        (if-let [fop (pu/first-open-point r-puzzle)] ;; at least one open point?
          (iterate-values r-puzzle solutions fop (str trail "->" fop))
          (save-puzzle-if solutions r-puzzle)))))) ;; nothing open? Done!


(defn start-solve [puzzle]
   "returns a collection of grids that are solutions for the puzzle, each having only one value for each grid cell, matching all criteria"
   ;; while there are points with more than one value: 
   ;; consider the first point of those, use it's first value to proceed: 
   ;; in each run: restrict the values in the new grid.
   ;;
  (util/reset-steps!)
  (let [solutions (trampoline solve-puzzle puzzle [] "")]
    (println "Steps" (util/get-steps) "\tnumber of solutions:" (count solutions))
    solutions))


