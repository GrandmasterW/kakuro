(ns kakuro.puzzle
  (:require [kakuro.point :as pt]
            [kakuro.cell :as ce]
            [kakuro.segment :as seg]
            [kakuro.util :as util]
            [clojure.pprint :as cpp]
            ))

;; ----------------------------------------------------------------------
;; A puzzle is a structure of a map of cells
;; and of a list of segments, which define
;; the sum-constraints. Puzzles cell values will start with 1,
;; but can be configured. 
;; ----------------------------------------------------------------------


(defrecord Puzzle [cells segments])


(defn count-potential-grids [cells]
  "count the number of potential grids: multiply the number of values of each cell"
  (reduce * 1 (map count (map :values cells))))




(defn get-puzzle-state [puzzle current-cell]
  "returns a state for the puzzle:
   :final, if all constraints are met and each cell value is just one digit
   :valid, if all constraints are met, but at least one cell as more than one potential values
   :invalid, if constraints are violated"
  ;;
  ;; check column and row of current-cell:
  ;; Are all cells set to one value only? and: all sum of row and column met exactly?
  ;; then check all rows and columns (TODO): :final
  ;; 
  ;; if not: if value sums <= col and row: :valid
  ;; else: :invalid
  :invalid
  )



(defn print-puzzle [puzzle]
  "Helper to print"
  (cpp/pprint puzzle)
  (println "Combinations to check: " (count-potential-grids (:cells puzzle))))

(defn create-puzzle [segments min max]
  "creates a puzzle as cells from the row segments in segments and from the segments" 
  (->Puzzle (ce/create-cells segments min max) segments))


(defn solve-puzzle [puzzle]
  "returns all viable solutions for a kakuro puzzle"
  (util/log (str "solve-puzzle: Starting to solve " (count-potential-grids (:cells puzzle)) " grids"))
  ;; take first cell that has more than one value
  ;; loop over them:
  ;; set cell value to one value
  ;; check if grid allows to continue, if so, continue with next cell
  ;; 
  
  )
