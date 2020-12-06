(ns kakuro.restrict.core
  (:require
   [kakuro.puzzle :as pu]
 ;  [kakuro.util :as util]
 ;  [kakuro.patterns :as pat]
 ;  [kakuro.logpuzzle :as lpu]
   [kakuro.dbgpuzzle :as dbp]
   [kakuro.grid :as gr]
   [kakuro.restrict.patterns :as rp]
   [kakuro.restrict.combis :as rc]
;   [clojure.math.combinatorics :as combo]
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
  (let [
        interim-grid (into {}
                           (mapv (partial restricter puzzle)
                                 (filter #(= (:orientation %1) orientation)
                                         (:segments puzzle))))

        ;; _ (do (println "RSO" "\tinterim-grid\t" interim-grid) nil )
        ]
    interim-grid))

(defn restrict-by
  "Restricts the grid cells values by considering each segment and packing the updated values back into a the puzzle.
   Each segment is restricted by restricter function. Horizontal segments first, puzzle updated for vertical segments. 
   Returns a puzzle."
  [puzzle restricter]

  (if (or (not (gr/is-correct-grid? (:grid puzzle)))
          (not (pu/is-open-puzzle? puzzle)))
    puzzle
    (let [
          ;; we have to consider one orientation first, update the puzzle and consider the other afterwards.
          ;; This ensures to work on incremental restrictions!
          ;; All segments of one orientation cannot influence each other - but h and v segments will do. 
          h-grid (restrict-seg-orientation puzzle :h restricter)

;;          _ (do (println "RBY" "\th-grid\t"  h-grid)            nil)

          h-puzzle (pu/merge-puzzle-grid puzzle h-grid)

          v-grid (restrict-seg-orientation h-puzzle :v restricter)

          ;; _ (do     (println "\tv-grid\t" v-grid)          nil)

          v-puzzle (pu/merge-puzzle-grid h-puzzle v-grid)

          ]
      v-puzzle)))

;;
;; putting it all together
;; 
(defn restrict-puzzle
  "loop over restrictions until nothing changes. Returns maximum restricted puzzle"
  [puzzle]
  (dbp/dbg-puzzle "restrict-puzzle#1" puzzle)

  (let [old-grid (:grid puzzle)

        ;; pr: pattern restricted
        pr-puzzle (restrict-by puzzle rp/restrict-segment-patterns)
        _ (do (dbp/dbg-puzzle "restrict-puzzle#2" pr-puzzle) nil)

        ;; improved by computing combinations
        com-puzzle (restrict-by pr-puzzle rc/restrict-segment-combis)
        _ (do (dbp/dbg-puzzle "restrict-puzzle#3" com-puzzle) nil)
        ]
    
    (if (= old-grid (:grid com-puzzle))
      (if (gr/is-correct-grid? (:grid com-puzzle)) com-puzzle puzzle)
      (recur com-puzzle))))

