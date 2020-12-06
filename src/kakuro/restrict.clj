(ns kakuro.restrict
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.util :as util]
   [kakuro.patterns :as pat]
   [kakuro.logpuzzle :as lpu]
   [kakuro.dbgpuzzle :as dbp]
   [kakuro.grid :as gr]
   [clojure.math.combinatorics :as combo]
   )
  )

;; ----------------------------------------------------------------------
;; Constants
;; ----------------------------------------------------------------------
;;
;; max number of places in range 0..8 for computing
;; combinations without blowing up the machine
;;
(def MAX_OPEN_COMBI 10)
;;
;; ----------------------------------------------------------------------
;;

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
;; pattern related functions
;; 

(defn get-deductible
  [cnt-open cnt-segpts grid segment]
  (if (= cnt-open cnt-segpts) 0 ; no need to compute
      (gr/segment-value-sum grid segment)))

(defn comp-vrange
  "compute the value range for the open points of a segment in puzzle by using the patterns. Returns a range set."
  [puzzle segment open-points]

  (let [grid (:grid puzzle)
        cnt-open (count open-points)
        cnt-segpts (count (:points segment))
        deductible (get-deductible cnt-open cnt-segpts grid segment)
        restsum (- (:sum segment) deductible)
        vmin (:min puzzle)
        vmax (:max puzzle)
        prange (pat/get-pattern restsum cnt-open vmin vmax)
        ]
    prange))

(defn restrict-segment-patterns
  "Returns a hash-map of points to values, where points are assigned to segment and values result from restrictions such as open segment sum"
  [puzzle segment]

  ;;(println "RSP" "\t" (:grid puzzle) "\n\tseg\t"(:points segment))
  (let [grid (:grid puzzle)
        open-points (gr/open-grid-points grid (:points segment))]

    (if (seq open-points)
      (let [vrange (comp-vrange puzzle segment open-points)

;;            _ (do (println  "\tvrange\t" vrange)                nil
            
            ;; assign to open points locally
            v-grid (into {} (mapv hash-map open-points (repeat vrange)))

;;            _ (do                (println  "\tv-grid\t" v-grid) nil)
            ]
        v-grid)
      {})))

;;
;; combinations
;;

(defn find-combi-fits
  "Retrieves the potential candidates the open-points value-sets
   which build the sum of the segment.
   Returns a list of tupels, in which first place belongs to first open-point and so on." 
  [grid segment open-points]
  (let [;; get open points value sets
        opvs (mapv #(get grid %1) open-points)
        
        ;; create cartesian product, one value of each set in a list element
        cp (apply combo/cartesian-product opvs)
            
        ;; keep lists with sum matching the segment sum
        fits (filter #(= (:sum segment) (apply + %1)) cp)           ]
    fits))


(defn find-transpose-remake
  "Returns a grid of the open points assigned to potential value sets from the combination. Nil if no combinations valid"
  [grid segment open-points]
  (let [fits (find-combi-fits grid segment open-points) ]
    
    (if (not (seq fits)) nil
        ;; else
        (into {}
              (map hash-map open-points 
                   (map #(into #{} %1) (util/transpose fits)))))))

(defn restrict-segment-combis
  "Restrict the combinations of values in a segment by using
  the cartesian product of the values.
  Achieved by transposing the results back into value-sets.
  Returns the open points with new value sets as a hash-map."
  [puzzle segment]
  
;;  (dbp/dbg-puzzle (str "RSC: " (:points segment)) puzzle)
  (let [grid (:grid puzzle)
        seg-points (:points segment)
        open-points (gr/open-grid-points grid seg-points)
        cnt-open (count open-points)]
    
    (if (or 
         (not (seq open-points))         
         (< cnt-open 2)                     ; do not waste time for one place only
         (> cnt-open MAX_OPEN_COMBI)) grid  ; do not compute for 9 places with 9 values...
        ;; else
        (if-let [change-grid (find-transpose-remake grid segment open-points)]
         ;; (do
;;            (dbp/dbg-puzzle (str "RSC:change-grid" (assoc puzzle :grid change-grid)))
;; to do: restrict not valid combinations, i.e. 6 and 6
          change-grid
         ;; )
          {}))))

;;
;; putting it all together
;; 
(defn restrict-puzzle
  "loop over restrictions until nothing changes. Returns maximum restricted puzzle"
  [puzzle]
  (dbp/dbg-puzzle "restrict-puzzle#1" puzzle)

  (let [old-grid (:grid puzzle)

        ;; pr: pattern restricted
        pr-puzzle (restrict-by puzzle restrict-segment-patterns)
        _ (do (dbp/dbg-puzzle "restrict-puzzle#2" pr-puzzle) nil)

        ;; improved by computing combinations
        com-puzzle (restrict-by pr-puzzle restrict-segment-combis)
        _ (do (dbp/dbg-puzzle "restrict-puzzle#3" com-puzzle) nil)
        ]
    
    (if (= old-grid (:grid com-puzzle))
      (if (gr/is-correct-grid? (:grid com-puzzle)) com-puzzle puzzle)
      (recur com-puzzle))))

