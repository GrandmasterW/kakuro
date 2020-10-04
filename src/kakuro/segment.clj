(ns kakuro.segment
  (:require [kakuro.point :refer :all][kakuro.cell :refer :all]))
;;  (:use [kakuro.point][kakuro.cell]))



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

(defn create-points-from-segment [segment]
  (let [type (:orientation segment)
        from-a (:from-a segment)
        to-a (:to-a segment)
        b (:b segment)]
    (case type
      :h (mapv #(->Point %1 b) (range from-a (inc to-a)))
      :v (mapv #(->Point %1 b) (range from-a (inc to-a)))
      [])))

