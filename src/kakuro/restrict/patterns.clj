(ns kakuro.restrict.patterns
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.util :as util]
   [kakuro.patterns :as pat]
   [kakuro.logpuzzle :as lpu]
   [kakuro.dbgpuzzle :as dbp]
   [kakuro.grid :as gr]
   )
  )


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

;;  (dbp/dbg-puzzle "RSP" puzzle)
;;  (util/log (str "RSP#segment\t" (:points segment)))
  
  (let [grid (:grid puzzle)
        open-points (gr/open-grid-points grid (:points segment))]

    (if (seq open-points)
      (let [vrange (comp-vrange puzzle segment open-points)

;;            _ (do (println  "\tvrange\t" vrange)                nil)
            
            ;; assign to open points locally
            v-grid (into {} (mapv hash-map open-points (repeat vrange)))

 ;;           _ (do                (println  "v-grid\t" v-grid) nil)
            ]
        v-grid)
      {})))

