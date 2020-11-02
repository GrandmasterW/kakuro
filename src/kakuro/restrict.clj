(ns kakuro.restrict
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.util :as util]
   [kakuro.point :as pt]
   [kakuro.grid :as grid]
   [kakuro.creation :as cr]
   [kakuro.segment :as seg]
   [clojure.set :as cs]
   )
  )


(defn comp-vmax [vmax restsum open-points]
  "Returns the value for maximum value range"
  (let [cnt-op (count open-points)
        red (max (dec cnt-op) 0) ;; to reduce for open cells, but min 0 
        cand-vmax (min vmax restsum) ;; works for one cell 
        cand-vmax2 (- cand-vmax red)]
;;    (util/log "comp-vmax" cnt-op               red              cand-vmax              cand-vmax2)
  (max cand-vmax2 1)))


(defn restrict-segment [puzzle segment]
  "Returns a hash-map of points to values, where points are assigned to segment and values result from restrictions such as open segment sum"
  ;; find open points, i.e. having more than one value
  (let [grid (:grid puzzle)
        segpoints (:points segment)
        open-points (grid/open-grid-points grid segpoints)
        deductible (if (= (count open-points)(count segpoints)) 0 ; no need to compute
                       (grid/segment-value-sum grid segment))
        restsum (- (:sum segment) deductible)
        vmin (:min puzzle)
        vmax (:max puzzle)
        amax (comp-vmax vmax restsum open-points) ; ensure minimum maximum... 
        vrange (cr/create-initial-value-set vmin amax)
        ]
;;    (util/log "restrict-segment"  grid  vrange)
    (into {}
          (map
           #(hash-map %1 (cs/intersection vrange (get grid %1)))
           segpoints))))


(defn reduce-grid-point [grid [point pt-values]]
  "If point is in grid, check if pt-values are less than existing values and use them if so."
  (if-let [grid-vals (get grid point)]
    (if (< (count pt-values) (count grid-vals))
      (assoc grid point pt-values) ; new values have fewer elements, use them
      grid) ; no need to change
    (assoc grid point pt-values))) ; old values do not exist, use new


(defn reduce-grid-part [grid grid-part]
  "To be used with reduce on the grid-parts: returns the extended new-grid by checking for each point in grid-part, if it is already in res-grid and updating res-grid only, if values of new point are less then the one in new-grid. So 1/1 #{1 2 3} will be updated by 1/1 #{1 2}. In fact, it is a encapsulated reduce again."
  (reduce reduce-grid-point grid grid-part))

(defn restrict-values [puzzle]
  "Restricts the grid cells values by considering each segment and packing the updated values back into a the puzzle"
  (let [grid (:grid puzzle)
        segments (:segments puzzle)
        grid-parts (map (partial restrict-segment puzzle) segments)
        new-grid (reduce reduce-grid-part {} grid-parts)]
    (assoc puzzle :grid new-grid)))

