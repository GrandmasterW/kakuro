(ns kakuro.grid
;;  (:require [kakuro.util :as util] [kakuro.cell :as cell] [kakuro.point :as pt][kakuro.segment :as seg])
  (:require [kakuro.util :as util]
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
  "Returns a string for the values: if only one digit is left, use that, preceeded by a point. If more than one, return # plus digit of number of values. Empty list: !!"
  [values]
  (cond
      (empty? values) "!!"
      ;; (= (count values) 1) (str (first values))
      :else (str values)))

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

(defn values-unique? 
  "true if each set contains only one digit, and all values of all sets appear only once"
  [value-sets]
  (and (every? (comp (partial = 1) count) value-sets)
       (every? (partial = 1) (vals (frequencies (mapv first value-sets))))))
  
(defn segment-values-unique? 
  "True, if each value of each segment point only appears once"
  [grid segment]
  (let [points (:points segment)
        value-sets (mapv (partial get grid) points)]
    (values-unique? value-sets)))

(defn is-correct-grid? 
  "True, if each point has at least one value"
  [grid]
  (let [value-sets (vals grid)]
    (every? (comp (partial < 0) count) value-sets)))
         
         
