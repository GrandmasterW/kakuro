(ns kakuro.grid
  (:require [kakuro.util :refer :all]
            [kakuro.constraints :as constr]))))

;; A kakuro grid is a matrix where each cell may contain values 1..9
;; A puzzle will contain restrictions for each row and for each column in terms of the sum which has to be fulfilled.
;; Hence a sum of 5 for two cells can be obtained by: (1 4) (2 3) (3 2) (4 1)
;; These functions construct an empty grid, the constraints for each row and column, and for each cell a set of valid digits.
;; Digits are less then the column sum and less then row sum.
;; To make it worse, each row or column can consist of individual segments, hence constraints apply for the segment.
;;
;; a point consists of coordinates: [1 2] where x in 1..xmax and y in 1..ymax
;; a cell consists of a point and the list of potential values, organized in the grid map:
;; { [1 1] [1 2 3 4 5 6 7 8 9]
;;   [2 1] [1 2 3 4 5 6 7 8 9]}

(defn create-point [x y]
  "creates one cell as a vector of coordinates"
  [x y])



(defn create-segment-points [[from-x to-x y]]
  "creates a vector of points from one row-segment"
  (mapv
   #(create-point %1 y)
   (range from-x (inc to-x))))

(defn create-points-for-segments [row-segments]
  "creates a vector of points, for each row-segment"
  (into []
        (mapcat create-segment-points row-segments)))

(defn create-initial-cells [row-segments min-value max-value]
  "creates a map with key point and a list of potential values, starting with min-value"
  (let [values (into [] (range min-value (inc max-value)))
        points (create-points-for-segments row-segments)]
    (reduce merge (map #(hash-map %1 values) points)))) 


(defn create-basic-grid [row-segments constraints]
  "creates a map containing the :cells as a map of cells and :constraints as a list of constraints"
  (let [initial-cells (create-initial-cells row-segments 1 9) ; change 1 and 9 to something later...
        cells (constr/apply-constraints initial-cells constraints)]
    (hash-map :cells cells :constraints constraints)))


