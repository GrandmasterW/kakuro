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
      (conj solutions solution-grid)
      solutions))

(defn save-puzzle-if [solutions puzzle]
  "Returns collection containing the grid of puzzle, if it is a valid puzzle, solutions otherwise"
  (if (pu/is-puzzle-valid? puzzle) 
    (collect-solution solutions (:grid puzzle)) ;; save it 
    solutions))

(defn puzzle-variations [puzzle point trail]
  "Returns a vector of puzzles, each containing a different value at point as single value"
  (let [values (get (:grid puzzle) point)]
;;    (util/log "puzzle-variations" (pt/str-point point) values)
    (mapv #(assoc-in puzzle [:grid point] #{%1}) values)))

(declare solve-puzzle)

(comment
(defn iterate-values [puzzle solutions first-open-point]
  "Walks through the values at first-open-point, fixes them for the point and goes into solve again"
  ;;
  (util/log "iterate-values:1" (pt/str-point first-open-point) (spz/puzzle-to-str puzzle))
  ;;
  (if-let [c-puzzles
           (filter (comp gr/is-correct-grid? :grid)
                   (puzzle-variations puzzle first-open-point))]
    (do
      ;;
      (util/log "iterate-values:2" (str "\n\t" (clojure.string/join "### \n\t" (spz/puzzle-to-str c-puzzles))))
      (let [v-solutions (remove empty? (mapcat #(solve-puzzle %1 []) c-puzzles))]
        (reduce conj solutions v-solutions)))))
)

(defn iterate-values [puzzle solutions first-open-point trail]
  "Walks through the values at first-open-point, fixes them for the point and goes into solve again"
  (if-let [c-puzzles (filter (comp gr/is-correct-grid? :grid)
                             (puzzle-variations puzzle first-open-point trail))]

    (let [v-solutions (remove
                       empty?
                       (for [p c-puzzles]
                         (solve-puzzle p [] (str trail "->" (pt/str-point p)))))]
      (reduce concat solutions v-solutions))
    solutions))

(defn solve-puzzle [puzzle solutions trail]
  {:pre [(not (util/count-steps!?))]}
  "Returns solutions, expanded with current, restricted puzzle, if it is a solution. Otherwise we go down to iterating the values at the first open point."
  ;;  (util/log "solve-puzzle" (spz/puzzle-to-str puzzle))
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
  (println "Potential solutions: " (pu/count-potential-solutions puzzle))
  (let [solutions (trampoline solve-puzzle puzzle [] "")]
    (println "Steps" (util/get-steps) "\tnumber of solutions:" (count solutions))
    solutions))


