(ns kakuro.point
  (:require [kakuro.util :as util]))

;; ----------------------------------------------------------------------
;; Definition of points in our grid: x: 1..n, y: 1..m, so not starting
;; with 0!
;; ----------------------------------------------------------------------


;;(defrecord Point [x y])

(defn Point 
  "Create a hashmap with x and y"
  [x y]
  {:x x :y y})

(defn str-point
  "returns a string for x and y in form x:y. Optionally takes sep to separate values"
  ([p]
   (str-point p ":")  )
  ([p sep] 
  (str (:x p) sep (:y p))))

(defn point-matches? 
  "true if val equals the value for ref being :x or :y. (point-matches? :x 4) is true for (Point. 4 3)"
  [ref val point]
  (= (ref point) val))

(defn- pt-between? [point a-determ b-determ from-a to-a b]
    (and (util/betweenl (a-determ point) from-a to-a)
         (= (b-determ point) b)))

(defn point-between? 
  "true if for coord :x from-a <= x <= to-a and y=b, for :y accordingly exchanged x and y"
  [point coord from-a to-a b]
  (case coord
     :x (pt-between? point :x :y from-a to-a b)
     :y (pt-between? point :y :x from-a to-a b)))
    
