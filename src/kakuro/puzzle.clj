(ns kakuro.puzzle
  (:require
   ;;[kakuro.util :as util]
   [kakuro.grid :as gr]
   [clojure.set :as cs]
   ;;[kakuro.point :as pt]
   ;;[clojure.pprint :as cpp]
   ))

;; ----------------------------------------------------------------------
;; A puzzle is a structure of a map of point -> cell (i.e. set of values)
;; and of a list of segments, which define
;; the sum-constraints.
;; Puzzles cell values will start with 1 for now
;; min equals minimum value range, max for the maximum
;; ----------------------------------------------------------------------


;; (defrecord Puzzle [grid segments min max])

(defn ->Puzzle  [grid segments min max]
  {:grid grid
   :segments segments
   :min min
   :max max})


(defn puzzle-dimensions
  "returns a vector of x and y maximum dimensions, i.e. the max x and max y value of contained points"
   [puzzle]
  (let [points (keys (:grid puzzle))]
    [(reduce max 0 (map :x points))
     (reduce max 0 (map :y points))]))

(defn count-potential-solutions
  "count the number of potential solutions by multipling the number of values of each cell"
   [puzzle]
  (reduce * 1N (map count (vals (:grid puzzle)))))

(defn is-open-puzzle?
  "True, if at least one open point on the grid"
   [puzzle]
  (seq (gr/open-grid-points (:grid puzzle))))

(defn first-open-point
  "Find the first open point on the puzzle, if any. Order them by x asc"
   [puzzle]
  (let [ops (gr/open-grid-points (:grid puzzle))]
    (if (seq ops)
      (first (sort-by first (sort-by second ops)))
      nil)))

(defn is-puzzle-valid?
  "True, if all constraints are met: segment sums equal value sums, digits in a segment are unique"
   [puzzle]
  (let [pgrid (:grid puzzle)
        segments (:segments puzzle)]
    (and
     (not (is-open-puzzle? puzzle)) ; cannot be open to be valid!
     (every? #(= (:sum %1) (gr/segment-value-sum pgrid %1)) segments) ; all sums met?
     (every? (partial gr/segment-values-unique? pgrid) segments)) ; all digits unique?
    ))

(defn is-correct-puzzle?
  "true if grid is correct and at least one segment exists"
  [puzzle]
  (and (gr/is-correct-grid? (:grid puzzle))
       (seq (:segments puzzle))))

(defn correct-puzzle-only
  [puzzle]
  (if (is-correct-puzzle? puzzle) puzzle nil))

(defn merge-puzzle-grid
  "If new-grid not nil, merges it by intersection
   with the grid of puzzle and assocs it into puzzle.
   Returns the new puzzle, but only if the new is correct, the incoming
   puzzle else."
  [puzzle new-grid]
  (if new-grid
    (let [m-puzzle
          (assoc puzzle
                 :grid
                 (merge-with cs/intersection (:grid puzzle) new-grid))]
      (if (is-correct-puzzle? m-puzzle) m-puzzle puzzle))
    puzzle))

