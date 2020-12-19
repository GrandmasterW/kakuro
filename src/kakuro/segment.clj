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

;;(defrecord Segment [orientation from-a to-a b sum points])

(defn ->Segment [orientation from-a to-a b sum points]
  {
   :orientation orientation
   :from-a from-a
   :to-a to-a
   :b b
   :sum sum
   :points points
   })


(defn- create-segment "helper to create empty set for points"
  ([orientation from-a to-a b sum]
   (create-segment orientation from-a to-a b sum nil))
  ([orientation from-a to-a b sum points]
   (->Segment orientation from-a to-a b sum (if points points #{}))))

(defn new-segment
  [orientation from-a to-a b sum]
  (create-segment orientation from-a to-a b sum))

(defn create-column-segment
  [from-y to-y x sum points]
  (create-segment :v from-y to-y x sum points))

(defn new-column
  [from-y to-y x sum]
  (create-column-segment from-y to-y x sum nil))

(defn create-row-segment [from-x to-x y sum points]
  (create-segment :h from-x to-x y sum points))

(defn new-row [from-x to-x y sum]
  (create-row-segment from-x to-x y sum nil))
;;
;; predicates
;;
(defn- matches-orientation? 
  "true, if orientation of segment equals orientation"
  [segment orientation]
  (= (:orientation segment) orientation))

(defn is-row-segment? 
  "true, if orientation is :h"
  [segment]
  (matches-orientation? segment :h))

(defn is-column-segment? 
  "true, if orientation is :v"
  [segment]
  (matches-orientation? segment :v))

(defn contains-point-in-set?
  "true, if point is in segment according to segments set"
  [point segment]
  (contains? (:points segment) point))


;; ----------------------------------------------------------------------
  
(defn segments-for-point
  "retrieves the 2 segments a point belongs to: one row, one column"
  [point segments]
  (filterv (partial contains-point-in-set? point) segments))


