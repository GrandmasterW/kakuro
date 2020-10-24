(ns kakuro.grid
;;  (:require [kakuro.util :as util] [kakuro.cell :as cell] [kakuro.point :as pt][kakuro.segment :as seg])
            )

;; ----------------------------------------------------------------------
;; A grid is simply a hashmap of point -> cell, as we need to retrieve
;; values of a cell by points.
;;
;; This file provides a set of helpers and access functions to deal with
;; grid stuff.
;; ----------------------------------------------------------------------


(defn make-empty-grid [] {})

(defn get-points-from-grid [grid]
  "returns a set of point values, i.e. the unique keys of grid"
  (into #{} (keys grid)))

(defn value-string [values]
  "returns a simple string for the values: if only one digit is left, use that. If more than one, return ?"
  (if (= (count values) 1)
    (str (first values))
    "?"))

(defn is-open-point? [grid point]
  "true, if point has more than one value in set"
  (> (count (get grid point)) 1))

(defn open-grid-points
  ([grid]
   "returns all open points from the grid"
   (open-grid-points grid (keys grid)))
  ([grid points]
   "returns the open points from the given collection"
   (filterv (partial is-open-point? grid) points)))

(defn fixed-grid-points
  ([grid]
   "returns all non-open points from the grid"
   (fixed-grid-points grid (keys grid)))
  ([grid points]
   "returns the non-open points from the given collection"
   (filterv (comp not (partial is-open-point? grid)) points)))

(defn segment-value-sum [grid segment]
  "computes the sum of the values which are non-open, i.e. set to a single value"
  (let [fixed (fixed-grid-points grid (:points segment))]
    (reduce + 0 (map (comp first (partial get grid)) fixed))))

