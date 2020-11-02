(ns kakuro.restrict
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.util :as util]
   [kakuro.patterns :as pat]
   [kakuro.point :as pt]
   [kakuro.grid :as gr]
   [kakuro.creation :as cr]
   [kakuro.segment :as seg]
   [clojure.set :as cs]
   )
  )

(defn comp-vmax [vmax restsum number-open]
  "Returns the value for maximum value range, based on vmax as defined value maximum in puzzle, restsum of segment and number-open open-points"
  (let [
        red (max (dec number-open) 0) ;; to reduce for open cells, but min 0 
        cand-vmax (min vmax restsum) ;; works for one cell 
        cand-vmax2 (- cand-vmax red)]
  (max cand-vmax2 1)))

(defn comp-vrange [puzzle segment open-points]
  "compute the value range for the open points of a segment in puzzle"
  (let [grid (:grid puzzle)
        cnt-open (count open-points)
        cnt-segpts (count (:points segment))
        deductible (if (= cnt-open cnt-segpts) 0 ; no need to compute
                       (gr/segment-value-sum grid segment))
        restsum (- (:sum segment) deductible)
        vmin (:min puzzle)
        vmax (:max puzzle)
        amax (comp-vmax vmax restsum cnt-open)
        prange (pat/get-unique-pattern restsum cnt-open)
        vrange (cr/create-initial-value-set vmin amax)]
    (if (and (< (count prange)(count vrange))
             (not (empty? prange)))
      prange
      vrange)))

(defn restrict-segment [puzzle segment]
  "Returns a hash-map of points to values, where points are assigned to segment and values result from restrictions such as open segment sum"
  (let [grid (:grid puzzle)
        segpoints (:points segment)
        open-points (gr/open-grid-points grid segpoints)
        vrange (comp-vrange puzzle segment open-points) ]
    (into {} (mapv
              #(hash-map %1 (cs/intersection vrange (get grid %1)))
              open-points)))) ;; TO DO make it better somehow. 

(defn restrict-values [puzzle]
  "Restricts the grid cells values by considering each segment and packing the updated values back into a the puzzle"
  {:pre [(gr/is-correct-grid? (:grid puzzle))]}
  (if (not (pu/is-open-puzzle? puzzle))
    puzzle
    (let [res-grid (into {}
                         (mapv
                          (partial restrict-segment puzzle)
                          (:segments puzzle)))
          new-grid (merge (:grid puzzle) res-grid)]
      (assoc puzzle :grid new-grid))))

