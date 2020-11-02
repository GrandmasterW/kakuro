(ns kakuro.puzzle
  (:require
   [kakuro.util :as util]
   [kakuro.grid :as gr]
   [kakuro.point :as pt]
   [clojure.pprint :as cpp]
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


(defn puzzle-dimensions [puzzle]
  "returns a vector of x and y maximum dimensions, i.e. the max x and max y value of contained points"
  (let [points (keys (:grid puzzle))]
    [(reduce max 0 (map :x points))
     (reduce max 0 (map :y points))]))

(defn count-potential-solutions [puzzle]
  "count the number of potential solutions by multipling the number of values of each cell"
  (reduce * 1 (map count (vals (:grid puzzle)))))


(defn is-open-puzzle? [puzzle]
  "True, if at least one open point on the grid"
  (not (empty? (gr/open-grid-points (:grid puzzle)))))

(defn first-open-point [puzzle]
  "Find the first open point on the puzzle, if any. Order them by x asc"
  (let [ops (gr/open-grid-points (:grid puzzle))]
    (if (and ops (not (empty? ops)))
      (first (sort-by first ops)))))

(defn is-puzzle-valid? [puzzle]
  "True, if all constraints are met: segment sums equal value sums, digits in a segment are unique"
  (let [pgrid (:grid puzzle)
        segments (:segments puzzle)]
    (and
     (not (is-open-puzzle? puzzle)) ; cannot be open to be valid!
     (every? #(= (:sum %1) (gr/segment-value-sum pgrid %1)) segments) ; all sums met?
     (every? (partial gr/segment-values-unique? pgrid) segments)) ; all digits unique?
    ))


