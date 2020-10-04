(ns kakuro.puzzle
  (:require [kakuro.point :refer :all]
            [kakuro.cell :refer :all]
            [kakuro.segment :refer :all]
            [kakuro.util :as util]
            ))

;; ----------------------------------------------------------------------
;; A puzzle is a structure of a map of cells, consisting of points and
;; their potential values, and of a list of segments, which define
;; the sum-constraints
;; ----------------------------------------------------------------------


(defrecord Puzzle [cells constraints])

(defn create-puzzle [row-segments constraints min max]
  "creates all the cells as defined by the row segments, each point is assigned the full range initially"
  (let [points (reduce into [] (mapv create-points-from-segment row-segments))
        cells (mapv ->Cell points (repeat (into [] (range min (inc max)))))]
    ;; @TODO limit cell values to min (sum) of row and column
    (util/log (str "Points: " points))
    (->Puzzle cells constraints)
    ))
