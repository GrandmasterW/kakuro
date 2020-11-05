(ns kakuro.strpuzzle
  (:require
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.util :as util]
   [kakuro.grid :as gr]
   [kakuro.puzzle :as pu]
   [clojure.pprint :as cpp]
   ))

(defn print-puzzle-stats [puzzle]
  "Helper to print"
  (let [grid (:grid puzzle)
        points (keys grid)
        dimensions (pu/puzzle-dimensions puzzle)]
  (println "Number cells:\t" (count points))
  (println "Grid points:\t" (clojure.string/join " " (map pt/str-point points)))
  (println "Max x:y:\t" (first dimensions) ":" (second dimensions))
  (println "Segments:\t" (count (:segments puzzle)))
  (println "Combinations to check:\t" (pu/count-potential-solutions puzzle))))

(defn point-to-str [grid x y]
  "return a string for point at x,y with the value or the number of values or a special character to indicate a non-cell" 
  (if-let [values (get grid (pt/Point x y))]
    (gr/value-string values)
    "="))

(defn puzzle-row-to-str [puzzle x-max row]
  "returns a string for the value of each point, but just one character per point"
  (let [pgrid (:grid puzzle)]
    (str
     (clojure.string/join
      "\t"
      (map
       #(point-to-str pgrid %1 row)
       (util/fullrange 1 x-max))))))

(defn puzzle-to-str [puzzle]
  "Returns a string of row-strings"
  (let [dimensions (pu/puzzle-dimensions puzzle)
        x-max (first dimensions)
        y-max (second dimensions)]
    (clojure.string/join
     "\t"
     (mapv
      (partial puzzle-row-to-str puzzle x-max)
      (util/fullrange 1 y-max)))))

(defn print-puzzle [puzzle]
  (println (puzzle-to-str puzzle)))



