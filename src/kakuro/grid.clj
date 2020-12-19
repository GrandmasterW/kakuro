(ns kakuro.grid
;;  (:require [kakuro.util :as util] [kakuro.cell :as cell] [kakuro.point :as pt][kakuro.segment :as seg])
  (:require
   [kakuro.util :as util]
   [kakuro.values :as vas]
   ))

;; ----------------------------------------------------------------------
;; A grid is simply a hashmap of point -> cell, as we need to retrieve
;; values of a cell by points.
;;
;; This file provides a set of helpers and access functions to deal with
;; grid stuff.
;; ----------------------------------------------------------------------


(defn make-empty-grid [] {})

(defn get-points-from-grid 
  "returns a set of point values, i.e. the unique keys of grid"
  [grid]
  (into #{} (keys grid)))

(defn value-string 
  "Returns a string for the values:
   if only one digit is left, use that.
   If more than one, return values as string
   For empty set: !"
  [values]
  (cond
      (empty? values) "!"
      (= (count values) 1) (str (first values))
      :else (str (sort values))))

(defn is-open-point? 
  "true, if point has more than one value in set"
  [grid point]
  (> (count (get grid point)) 1))

(defn open-grid-points
  "returns all open points from the grid, or returns a collection of open points from the given collection"
  ([grid]
   (open-grid-points grid (keys grid)))
  ([grid points]
   (filterv (partial is-open-point? grid) points)))

(defn fixed-grid-points
  "returns all non-open points from the grid, or returns the non-open points from the given collection"
  ([grid]
   (fixed-grid-points grid (keys grid)))
  ([grid points]

   (filterv (comp not (partial is-open-point? grid)) points)))

(defn segment-value-sum 
  "computes the sum of the values which are non-open, i.e. set to a single value"
  [grid segment]
  (let [fixed (fixed-grid-points grid (:points segment))]
;;    (util/log "segment-value-sum" grid fixed)
    (if fixed
      (reduce + 0
              (map (comp (partial util/first-val 0) (partial get grid)) fixed))
      0)))


(defn points-values-unique? 
  "True, if each value of the given points only appears once.
   To be used in a segment, makes no sense in a whole grid!"
  [grid points]
  (let [value-sets (mapv (partial get grid) points)]
    (vas/values-unique? value-sets)))

(defn all-segment-values-unique?
  "True, for if all points have unique values in a segment and have one value only."
  [grid segment]
  (let [points (:points segment)]
    (and
     (every? #(= (count %1) 1) (vas/get-value-sets grid points))
     (points-values-unique? grid points))))
  
(defn fixed-segment-values-unique?
  "True, if the values of the fixed points in the segment are unique"
  [grid segment]
  (points-values-unique? grid (fixed-grid-points grid (:points segment))))
  
(defn is-correct-grid? 
  "True, if each point has at least one value"
  [grid]
  (let [value-sets (vals grid)]
    (every? (comp (partial < 0) count) value-sets)))
         

