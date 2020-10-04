(ns kakuro.point)

;; ----------------------------------------------------------------------
;; Definition of points in our grid: x: 1..n, y: 1..m, so not starting
;; with 0!
;; ----------------------------------------------------------------------

(defrecord Point [x y])

