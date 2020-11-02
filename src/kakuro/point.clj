(ns kakuro.point
  (:require [kakuro.util :as util]))

;; ----------------------------------------------------------------------
;; Definition of points in our grid: x: 1..n, y: 1..m, so not starting
;; with 0!
;; ----------------------------------------------------------------------


;;(defrecord Point [x y])

(defn Point [x y]
  "Create a hashmap with x and y"
  {:x x :y y})

(defn str-point
  ([p]
   "returns a string for x and y in form x y"
   (str-point p ":")  )
  ([p sep] "uses sep to separate values"
  (str (:x p) sep (:y p))))

(defn point-matches? [ref val point]
  "true if val equals the value for ref being :x or :y. (point-matches? :x 4) is true for (Point. 4 3)"
  (= (ref point) val))

(defn- pt-between? [point a-determ b-determ from-a to-a b]
    (and (util/betweenl (a-determ point) from-a to-a)
         (= (b-determ point) b)))

(defn point-between? [point coord from-a to-a b]
  "true if for coord :x from-a <= x <= to-a and y=b, for :y accordingly exchanged x and y"
   (case coord
     :x (pt-between? point :x :y from-a to-a b)
     :y (pt-between? point :y :x from-a to-a b)))
    
