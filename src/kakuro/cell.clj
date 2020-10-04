(ns kakuro.cell
  (:use [kakuro.point]))


;; ----------------------------------------------------------------------
;; Definition of cells
;;
;; A cell is a combination of a point and a list of potential values
;; ----------------------------------------------------------------------

;; contains a Point and a list of potential values. values should be a vector.
(defrecord Cell [point values])

