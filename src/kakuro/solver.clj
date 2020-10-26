(ns kakuro.solver
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.util :as util]
   [kakuro.point :as pt]
   [kakuro.grid :as grid]
   [kakuro.creation :as cr]
   [kakuro.segment :as seg]
   )
  )

(defn restrict-segment [puzzle segment]
  "Returns a hash-map of points to values, where points are assigned to segment and values result from restrictions such as open segment sum"
  ;; find open points, i.e. having more than one value
  (let [grid (:grid puzzle)
        segpoints (:points segment)
        open-points (grid/open-grid-points grid segpoints)
        deductible (if (= open-points segpoints) ; to need to compute
                     0
                     (grid/segment-value-sum grid segment))
        restsum (- (:sum segment) deductible)
        vmin (:min puzzle)
        vmax (:max puzzle)
        amax (max (dec (min vmax restsum)) 1) ; ensure minimum maximum... 
        vrange (cr/create-initial-value-set vmin amax)
        ]
    (into {} (map #(hash-map %1 vrange) segpoints))))


(defn reduce-grid-point [grid [point pt-values]]
  "If point is in grid, check if pt-values are less than existing values and use them if so."
  (if-let [grid-vals (get grid point)]
    (if (< (count pt-values) (count grid-vals))
      (assoc grid point pt-values) ; new values have fewer elements, use them
      grid) ; no need to change
    (assoc grid point pt-values))) ; old values do not exist, use new


(defn reduce-grid-part [grid grid-part]
  "To be used with reduce on the grid-parts: returns the extended new-grid by checking for each point in grid-part, if it is already in res-grid and updating res-grid only, if values of new point are less then the one in new-grid. So 1/1 #{1 2 3} will be updated by 1/1 #{1 2}. In fact, it is a encapsulated reduce again."
  (reduce reduce-grid-point grid grid-part))

(defn restrict-values [puzzle]
  "Restricts the grid cells values by considering each segment and packing the updated values back into a the puzzle"
  (let [grid (:grid puzzle)
        segments (:segments puzzle)
        grid-parts (map (partial restrict-segment puzzle) segments)
        new-grid (reduce reduce-grid-part {} grid-parts)]
    (assoc puzzle :grid new-grid)))

(declare solve)

(defn collect-solutions [solutions new-solutions]
  "Returns a cleaned vector of solutions, enriched by new-solutions"
  (if (and new-solutions (not (empty? new-solutions)))
    (concat solutions new-solutions)
    solutions))
  
(defn iterate-values-at [puzzle solutions first-open-point]
  "Walks through the values at first-open-point, fixes them for the point and goes into solve again"
  (let [values (get (:grid puzzle) first-open-point)
        v-puzzles (map #(assoc-in puzzle [:grid first-open-point] #{%1}) values)
        v-solutions (map #(solve %1 solutions) v-puzzles)]
    (collect-solutions solutions v-solutions)))

(defn iterate-open-points [puzzle solutions open-points]
  "Walks through the open points and computes them at each step"
  (if (or (nil? open-points)(empty? open-points))
    solutions
    (recur
     puzzle
     (collect-solutions solutions (iterate-values-at puzzle solutions (first open-points)))
     (grid/open-grid-points (:grid puzzle)))))

(defn solve
  ([puzzle]
   "returns a collection of grids that are solutions for the puzzle, each having only one value for each grid cell, matching all criteria"
   (let [start-puzzle (restrict-values puzzle)]
     ;; while there are points with more than one value: 
     ;; consider the first point of those, use it's first value to proceed: 
     ;; in each run: restrict the values in the new grid.
     ;;
     (trampoline solve start-puzzle [])))
  ([puzzle solutions]
   "worker"
   (if (pu/is-open-puzzle? puzzle) ;; shall we continue?
     (iterate-open-points puzzle solutions (grid/open-grid-points (:grid puzzle)))
     (if (pu/is-final-valid? puzzle) ;; abort the puzzle, as it is no longer open. 
       (collect-solutions solutions [(:grid puzzle)]) ;; save it 
       solutions))) ;; else just stop and return
  ) ; the end. 
     

