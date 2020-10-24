(ns kakuro.segment
  (:require [kakuro.point :as pt]
            ))

;; ----------------------------------------------------------------------
;; A segment is a part of a row or a column, stretching from from-a to
;; to-a and having row or column b.
;; Moreover, a segment will have a sum value indicating the target sum of
;; all cells in that segment.
;; orientation is :h for row horizontal, :v for column vertical
;;
;; points contains a set (!) of points that belong to the segment to speed
;; up lookup when processing solutions
;; ----------------------------------------------------------------------

(defrecord Segment [orientation from-a to-a b sum points])

(defn- create-segment [orientation from-a to-a b sum points]
  "helper to create empty set for points"
  (Segment. orientation from-a to-a b sum (if points points #{})))

(defn create-column-segment [from-y to-y x sum points]
  (create-segment :v from-y to-y x sum points))

(defn create-row-segment [from-x to-x y sum points]
    (create-segment :h from-x to-x y sum points))
;;
;; predicates
;;
(defn- matches-orientation? [segment orientation]
  "true, if orientation of segment equals orientation"
  (= (:orientation segment) orientation))

(defn is-row-segment? [segment]
  "true, if orientation is :h"
  (matches-orientation? segment :h))

(defn is-column-segment? [segment]
  "true, if orientation is :v"
  (matches-orientation? segment :v))

(defn contains-point-in-set? [point segment]
  "true, if point is in segment according to segments set"
  (contains? (:points segment) point))

;; ----------------------------------------------------------------------
  
(defn segments-for-point [point segments]
  "retrieves the 2 segments a point belongs to: one row, one column"
  (filterv (partial contains-point-in-set? point) segments))


