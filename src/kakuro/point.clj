(ns kakuro.point)

;; ----------------------------------------------------------------------
;; Definition of points in our grid: x: 1..n, y: 1..m, so not starting
;; with 0!
;; ----------------------------------------------------------------------

(defrecord Point [x y])

(defn point-matches? [ref val point]
  "true if val equals the value for ref being :x or :y. (point-matches? :x 4) is true for (Point. 4 3)"
  (= (ref point) val))
