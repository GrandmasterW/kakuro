(ns kakuro.restrict
  (:require
   [kakuro.puzzle :as pu]
   ;;   [kakuro.util :as util]
   [kakuro.patterns :as pat]
   ;; [kakuro.point :as pt]
   [kakuro.grid :as gr]
   [kakuro.creation :as cr]
   ;;[kakuro.segment :as seg]
   [clojure.set :as cs]
   [clojure.math.combinatorics :as combo]
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
                 (comp-vrange puzzle segment open-points) ;; this is too broad - should be limited to a point? 
                 #{})]
    (into {} (mapv
              #(hash-map %1 (cs/intersection vrange (get grid %1)))
              open-points)))) ;; TO DO make it better somehow. 

(defn restrict-segment-combis
  "restrict the combinations of values in a segment by using the cartesian product of the values"
  [puzzle segment]
  (let [grid (:grid puzzle)
        segpoints (:points segment)
        open-points (gr/open-grid-points grid segpoints)
        opvs (mapv #(get grid %1) open-points)
        cp (apply combo/cartesian-product opvs)]
    ;; filter sum of each element equal segment sum
    ;; transpose back to values
    ;; assign to open-points
    ;; avoid 1 place treatment
    ;; TODO
    cp
  ))




(defn restrict-values
  "Restricts the grid cells values by considering each segment and packing the updated values back into a the puzzle"
  [puzzle]
;;  {:pre [(gr/is-correct-grid? (:grid puzzle))]}

  (if (not (pu/is-open-puzzle? puzzle))
    puzzle
    (let [res-grid (into {}
                         (mapv
                          (partial restrict-segment puzzle)
                          (:segments puzzle)))
          new-grid (merge (:grid puzzle) res-grid)]
      (assoc puzzle :grid new-grid))))



  
(defn restrict-puzzle
  "loop over restrictions until nothing changes. Returns maximum restricted puzzle"
  [puzzle]
  (let [old-grid (:grid puzzle)
        new-puzzle (restrict-values puzzle)]
    (if (= old-grid (:grid new-puzzle))
      ;; no changes any more?
      new-puzzle ;; done!
      ;; else: try one more restriction
      (recur new-puzzle))))

