(ns kakuro.segment
  (:require [kakuro.point :as pt]
            [kakuro.util :as util]))

;; ----------------------------------------------------------------------
;; A segment is a part of a row or a column, stretching from from-a to
;; to-a and having row or column b.
;; Moreover, a segment will have a sum value indicating the target sum of
;; all cells in that segment.
;; orientation is :h for row horizontal, :v for column vertical
;; ----------------------------------------------------------------------


(defrecord Segment [orientation from-a to-a b sum])

(defn column-segment [from-y to-y x sum]
  (Segment. :v from-y to-y x sum))

(defn row-segment [from-x to-x y sum]
  (Segment. :h from-x to-x y sum))

(defn is-row-segment? [segment]
  "true, if orientation is :h"
  (= (:orientation segment) :h))


(defn pt-on-seg? [point segment pt-a pt-b]
  "helper predicate as generic implementation with provided retrievers"
  (and
   (= (pt-b point) (:b segment))
   (util/betweenl (pt-a point) (:from-a segment)(:to-a segment))))

(defn contains-point? [point segment]
  "true, if point is in segment"
  (case (:orientation segment)
    :h (pt-on-seg? point segment :x :y)
    :v (pt-on-seg? point segment :y :x)
    ))
  
(defn point-segments [point segments]
  "retrieves the potential 2 segments a point belongs to: one row, one column"
  (filterv (partial contains-point? point) segments))


(defn create-points-from-segment [segment]
  (let [type (:orientation segment)
        from-a (:from-a segment)
        to-a (:to-a segment)
        b (:b segment)]
    (case type
      :h (mapv #(pt/->Point %1 b) (range from-a (inc to-a)))
      :v (mapv #(pt/->Point b %1) (range from-a (inc to-a)))
      [])))

