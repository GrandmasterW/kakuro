(ns kakuro.strpuzzle
  (:require
   [kakuro.point :as pt]
   ;;   [kakuro.segment :as seg]
   [kakuro.util :as util]
   [kakuro.grid :as gr]
   [kakuro.puzzle :as pu]
   [clojure.string :as cst]   ))


(defn point-to-str 
  "return a string for point at x,y with the value or the number of values or a special character to indicate a non-cell"
  [grid x y]
  (if-let [values (get grid (pt/Point x y))]
    (gr/value-string values)
    "-"))

(defn puzzle-row-to-str 
  "returns a string for the value of each point, but just one character per point"
  [puzzle x-max row]
  (let [pgrid (:grid puzzle)]
    (cst/join
     " "
     (map
      #(point-to-str pgrid %1 row)
      (util/fullrange 1 x-max)))))

(defn puzzle-to-str 
  "Returns a string of row-strings"
  [puzzle]
  (let [dimensions (pu/puzzle-dimensions puzzle)
        x-max (first dimensions)
        y-max (second dimensions)]
    (cst/join
     "\n|"
     (mapv
      (partial puzzle-row-to-str puzzle x-max)
      (util/fullrange 1 y-max)))))

(defn print-puzzle [puzzle]
  (println (puzzle-to-str puzzle)))


(defn values-str 
  "convert value set into readable string"
  [vs]
  (cst/join " " (sort (into [] vs))))


