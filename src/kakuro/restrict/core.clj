(ns kakuro.restrict.core
  (:require
   [kakuro.puzzle :as pu]
   ;;  [kakuro.util :as util]
   [kakuro.logpuzzle :as lpu]
   [kakuro.dbgpuzzle :as dbp]
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
  (let [
        interim-grid (into {}
                           (mapv (partial restricter puzzle)
                                 (filter #(= (:orientation %1) orientation)
                                         (:segments puzzle))))

        ;; _ (println "RSO" "\tinterim-grid\t" interim-grid)
        ]
    interim-grid))

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

;;          _ (do (println "RBY" "\th-grid\t"  h-grid)            nil)

          h-puzzle (pu/merge-puzzle-grid puzzle h-grid)

          v-grid (restrict-seg-orientation h-puzzle :v restricter)

          ;; _ (do     (println "\tv-grid\t" v-grid)          nil)

          v-puzzle (pu/merge-puzzle-grid h-puzzle v-grid)

          ]
      v-puzzle)))


(defn lp
  "Helper to log puzzle with puzzle as first parameter, returns puzzle"
  [puzzle msg]
  (lpu/log-puzzle msg puzzle)
  puzzle)

(defn restrict-loop
  "restricts the puzzle by restricter until results do not differ any more. Returns a puzzle."
  [puzzle restricter]
  (let [interim (restrict-by puzzle restricter)]
    (if (= puzzle interim) interim ; done
        (recur interim restricter))))

;;
;; putting it all together
;; 
(defn restrict-puzzle
  "loop over restrictions until nothing changes. Returns maximum restricted puzzle. If puzzle is not valid, return nil. "
  [puzzle]

;;  (util/log "restrict-puzzle" "start")

  (let [
        result (-> puzzle
                   (restrict-loop rp/restrict-segment-patterns)
                   ;; (lp "pr")
                   (restrict-loop rc/restrict-segment-combis)
                   ;; (lp "cr")
                   )
        ]
;;    (dbp/dbg-puzzle "restrict-puzzle#2" result)
    (if (= result puzzle)
      result
      (recur result))))
  
