(ns kakuro.puzzle
  (:require
   [kakuro.grid :as gr]
   [clojure.set :as cs]
   [kakuro.segment :as seg]
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
  "True-ish, if at least one open point on the grid"
   [puzzle]
  (seq (gr/open-grid-points (:grid puzzle))))

(defn first-open-point
  "Find the first open point on the puzzle, if any. Order them by x asc"
   [puzzle]
  (let [ops (gr/open-grid-points (:grid puzzle))]
    (if (seq ops)
      (first (sort-by first (sort-by second ops)))
      nil)))


(defn all-segment-sums-done?
  "True, if for each segment the sum equals the value in the segment."
  [grid segments]
  (every?
   #(= (:sum %1) (gr/segment-value-sum grid %1))
   segments))

(defn is-puzzle-solution?
  "True, if all constraints are met: segment sums equal value sums, digits in a segment are unique"
   [puzzle]
  (let [pgrid (:grid puzzle)
        segments (:segments puzzle)]
    (and
     (not (is-open-puzzle? puzzle)) ; cannot be open to be valid!
     (all-segment-sums-done? pgrid segments)
     (every? (partial gr/all-segment-values-unique? pgrid) segments)) ; all digits unique?
    ))

(defn is-correct-puzzle?
  "true if grid is correct and at least one segment exists"
  [puzzle]
  (and (gr/is-correct-grid? (:grid puzzle))
       (> (count (:segments puzzle)) 0)))

(defn distinct-fixed-values?
  "true, if for each segment the values of its fixed points are distinct." 
  [puzzle]
  (let [fixed-points (mapv #(gr/fixed-grid-points (:grid puzzle) (:points %1)) (:segments puzzle))]
  (every?
   #(gr/points-values-unique? (:grid puzzle) %1)
   fixed-points)))

(defn assert-points-match
  "Returns the puzzle, if it contains the same number of points
   defined by rows as by columns. Throws an exception if not."
  [puzzle]
  (let [row-col-points (seg/count-point-number (:segments puzzle))
        h-num (:h row-col-points)
        c-num (:v row-col-points)
        ]
    (if-not (= h-num c-num)
      (throw
       (AssertionError.
        (str "Number of defined points in rows and columns differ: rows: "
             h-num
             ", columns: "
             c-num
             ". Check your segments!" )))
      puzzle)))

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

