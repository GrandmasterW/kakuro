(ns kakuro.restrict.core
  (:require
   [kakuro.puzzle :as pu]
   ;;  [kakuro.util :as util]
   [kakuro.logpuzzle :as lpu]
   ;;   [kakuro.dbgpuzzle :as dbp]
   ;;   [kakuro.grid :as gr]
   [kakuro.restrict.patterns :as rp]
   [kakuro.restrict.combis :as rc]
   )
  )

;;
;; for both, patterns and combinations
;;
(defn restrict-seg-orientation
  "Generic function, applied to a puzzle a given orientation and a restricter function.
   Applies restricter to all segments with orientation, i.e. restricter must take puzzle and segment.
   Returns a grid of point to value-sets of the restricter. "
  [puzzle orientation restricter]
  (->> puzzle
       (:segments)
       (filter #(= (:orientation %1) orientation))
       (mapv (partial restricter puzzle))
       (into {})))

(defn restrict-by
  "Restricts the grid cells values by considering each segment and packing the updated values back into a the puzzle.
   Each segment is restricted by restricter function. Horizontal segments first, puzzle updated for vertical segments. 
   Returns a puzzle."
  [puzzle restricter]

  (if (or (not (pu/is-correct-puzzle? puzzle))
          (not (pu/is-open-puzzle? puzzle)))
    puzzle
    (let [
          ;; we have to consider one orientation first, update the puzzle and consider the other afterwards.
          ;; This ensures to work on incremental restrictions!
          ;; All segments of one orientation cannot influence each other - but h and v segments will do. 
          h-grid (restrict-seg-orientation puzzle :h restricter)

          h-puzzle (pu/merge-puzzle-grid puzzle h-grid)

          v-grid (restrict-seg-orientation h-puzzle :v restricter)

          v-puzzle (pu/merge-puzzle-grid h-puzzle v-grid)
          ]
      v-puzzle)))

(defn restrict-loop
  "Restricts the puzzle by restricter until results do not differ any more. Returns a puzzle."
  [puzzle restricter]
  (let [interim (restrict-by puzzle restricter)]

    (if (= puzzle interim)

      interim ; done

      (recur interim restricter))))

;;
;; putting it all together
;; 
(defn restrict-puzzle
  "Loop over restrictions until nothing changes. Returns maximum restricted puzzle. If puzzle is not valid, return nil. "
  [puzzle]

  (let [
        result (-> puzzle
                   (restrict-loop rp/restrict-segment-patterns)
                   (restrict-loop rc/restrict-segment-combis))
        ]
    (if (= result puzzle)
      result
      (recur result))))
  
