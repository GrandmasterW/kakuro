(ns kakuro.cell
  (:require [kakuro.point :as pt]
            [kakuro.segment :as seg]
            [kakuro.cell :as ce]
            [kakuro.util :as util]))


;; ----------------------------------------------------------------------
;; Definition of cells
;;
;; A cell is a combination of a point and a list of potential values
;; ----------------------------------------------------------------------

;; contains a Point and a list of potential values. values should be a vector.
(defrecord Cell [point values])

(defn- cells-finder [cells dim val]
  "helper to find cells matching given point element as :x or :y, returns vector of cells"
  (into [] (filter #(pt/point-matches? dim val (:point %1)) cells)))

(defn cells-in-row [cells row]
  "returns a vector of all cells in cells having y = row"
  (cells-finder cells :y row))

(defn cells-in-column [cells column]
  "returns a vector of all cells in cells having x = column"
  (cells-finder cells :x column))
  
(defn min-segment-sum [point segments]
  "returns the minimum of all sums of all the segments a point is part of"
  ;; filter segments to contain only ones with point element segment
  ;; map get sums
  ;; min maps
  (let [p-seg (seg/point-segments point segments)
        sums (map :sum p-seg)
        res (reduce min sums)]
    res))


(defn make-cell-for-point [segments v-min v-max point]
  "create a cell for a point, assigning the value range that has minimum sum of column and row"
  (->Cell
   point
   (util/fullrange v-min (min (ce/min-segment-sum point segments) v-max))))

(defn create-cells [segments v-min v-max]
  "creates the cells from all row-segments in range min to max values. Returns a vector."
  (let [row-segments (filter seg/is-row-segment? segments)
        points (reduce into [] (mapv seg/create-points-from-segment row-segments))]
    (mapv (partial make-cell-for-point segments v-min v-max) points)))


