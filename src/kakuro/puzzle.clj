(ns kakuro.puzzle
  (:require
   ;;   [kakuro.point :as pt] [kakuro.segment :as seg]
   [kakuro.util :as util]
   [kakuro.grid :as grid]
   [kakuro.point :as pt]
   [clojure.pprint :as cpp]
   ))

;; ----------------------------------------------------------------------
;; A puzzle is a structure of a map of point -> cell (i.e. set of values)
;; and of a list of segments, which define
;; the sum-constraints.
;; Puzzles cell values will start with 1 for now
;; min equals minimum value range, max for the maximum
;; ----------------------------------------------------------------------


(defrecord Puzzle [grid segments min max])

(defn puzzle-dimensions [puzzle]
  "returns a vector of x and y maximum dimensions, i.e. the max x and max y value of contained points"
  (let [points (keys (:grid puzzle))]
    [(reduce max (map :x points))
     (reduce max (map :y points))]))

(defn count-potential-solutions [puzzle]
  "count the number of potential solutions by multipling the number of values of each cell"
  (reduce * 1 (map count (vals (:grid puzzle)))))

(defn print-puzzle-stats [puzzle]
  "Helper to print"
  (let [grid (:grid puzzle)
        points (keys grid)
        dimensions (puzzle-dimensions puzzle)]
  (println "Number cells:\t" (count points))
  (println "Grid points:\t" (clojure.string/join " " (map pt/str-point points)))
  (println "Max x:y:\t" (first dimensions) ":" (second dimensions))
  (println "Segments:\t" (count (:segments puzzle)))
  (println "Combinations to check:\t" (count-potential-solutions puzzle))))

(defn- str-pt [grid points x y]
  "return a string for point at x,y: # if not in grid, digit or ? if in grid"
  (let [p (pt/->Point x y)]
    (if (contains? points p)
      (grid/value-string (get grid p))
      "#")))

(defn print-puzzle [puzzle]
  "creates strings for each row and prints them line by line"
  (let [dimensions (puzzle-dimensions puzzle)
        x-max (first dimensions)
        grid (:grid puzzle)
        points (into #{} (keys grid))]
    (dotimes [c (first dimensions)]
      (println (clojure.string/join (map #(str-pt grid points %1 (inc c)) (util/fullrange 1 x-max)))))))
      






