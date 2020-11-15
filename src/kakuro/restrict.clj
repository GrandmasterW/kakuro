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

(defn comp-vmax
  "Returns the value for maximum value range,
  based on vmax as defined value maximum in puzzle,
  restsum of segment and number-open open-points.
  If number-open is 1: min vmax, restsum
  If restsum < vmax: restsum -1 as you cannot use the digit for restsum itself
  else vmax because we cannot judge which to spare (could be extended by combinatorics with vmax and number-open)"
  [vmax restsum number-open]
  {:pre [(> restsum 0)]}
  (cond
    (= number-open 1) (min vmax restsum)
    (< restsum vmax) (dec restsum)
    :else vmax))


(defn make-vrange
  [vmin vmax restsum cnt-open]
  (cond
    (or (= restsum 0)(= cnt-open 0)) #{} ;; TO DO: somehow better
    (= cnt-open 1)                   #{(min vmax restsum)}
    :else (cr/create-initial-value-set vmin (comp-vmax vmax restsum cnt-open))))

(defn comp-vrange
  "compute the value range for the open points of a segment in puzzle"
  [puzzle segment open-points]
  {:pre [(seq open-points)]}
  (let [grid (:grid puzzle)
        cnt-open (count open-points)
        cnt-segpts (count (:points segment))
        deductible (if (= cnt-open cnt-segpts) 0 ; no need to compute
                       (gr/segment-value-sum grid segment))
        restsum (- (:sum segment) deductible)
        vmin (:min puzzle)
        vmax (:max puzzle)
        prange (pat/get-pattern restsum cnt-open)
        vrange (make-vrange vmin vmax restsum cnt-open)]
    (comment
      (util/log grid)      (util/log cnt-open)      (util/log cnt-segpts)      (util/log deductible)      (util/log restsum)
      (util/log vmin)      (util/log vmax)      (util/log amax)      (util/log prange)      (util/log vrange))
    
    (if (and (< (count prange)(count vrange))(seq prange))
      prange
      vrange)))

(defn restrict-segment
  "Returns a hash-map of points to values, where points are assigned to segment and values result from restrictions such as open segment sum"
  [puzzle segment]
  (let [grid (:grid puzzle)
        segpoints (:points segment)
        open-points (gr/open-grid-points grid segpoints)
        vrange (if (seq open-points)
                 (comp-vrange puzzle segment open-points)
                 #{})]
    (comment      (util/log grid)      (util/log open-points)      (util/log vrange))
    (into {} (mapv
              #(hash-map %1 (cs/intersection vrange (get grid %1)))
              open-points)))) ;; TO DO make it better somehow. 

(defn restrict-values
  "Restricts the grid cells values by considering each segment and packing the updated values back into a the puzzle"
  [puzzle]
;;  {:pre [(gr/is-correct-grid? (:grid puzzle))]}
;;  (util/log (:grid puzzle))
  (if (not (pu/is-open-puzzle? puzzle))
    puzzle
    (let [res-grid (into {}
                         (mapv
                          (partial restrict-segment puzzle)
                          (:segments puzzle)))
          new-grid (merge (:grid puzzle) res-grid)]
      (assoc puzzle :grid new-grid))))

