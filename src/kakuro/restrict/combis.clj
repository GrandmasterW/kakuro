(ns kakuro.restrict.combis
  (:require
   [kakuro.util :as util]
   [kakuro.logpuzzle :as lpu]
   [kakuro.dbgpuzzle :as dbp]
   [kakuro.grid :as gr]
   [clojure.math.combinatorics :as combo]
   [clojure.string :as s]
   )
  )

;; ----------------------------------------------------------------------
;; Constants
;; ----------------------------------------------------------------------
;;
;; max number of places for computing
;; combinations without blowing up the machine
;;
(def MAX_OPEN_COMBI 9)
;;
;; ----------------------------------------------------------------------
;;

(defn valid-combi?
  "true, if sum of coll elements equal restsum and all elements of
   coll are distinct."
  [restsum coll]
  (and (= restsum (apply + coll))
       (apply distinct? coll)))

(defn find-combi-fits
  "Retrieves the potential candidates the open-points value-sets
   which build the sum of the segment.
   Returns a list of tupels, in which first place belongs to first open-point and so on." 
  [grid segment open-points]
  (let [;; get open points value sets
        opvs (mapv #(get grid %1) open-points)

        ;; _ (println "opvs" opvs)
        
        ;; create cartesian product, one value of each set in a list element
        cp (apply combo/cartesian-product opvs)

        ;; _ (println "cp" cp)
        
        ;; get open sum to look for
        restsum (- (:sum segment) (gr/segment-value-sum grid segment))

        ;;_ (println "restsum" restsum)
        
        ]
    (filter (partial valid-combi? restsum) cp)))

(defn find-transpose-remake
  "Returns a grid of the open points assigned to potential value sets from the combination. Nil if no combinations valid"
  [grid segment open-points]
  ;; (dbp/dbg-grid "ftr" grid)   (println "open:" open-points)
  (let [fits (find-combi-fits grid segment open-points) ]

    ;;    (util/log "ftr:fits" (s/join " / " fits))

    (if-not (seq fits)
      nil
      (into {}
            (map hash-map open-points 
                 (map #(into #{} %1) (util/transpose fits)))))))

(defn restrict-segment-combis
  "Restrict the combinations of values in a segment by using
  the cartesian product of the values.
  Achieved by transposing the results back into value-sets.
  Returns the open points with new value sets as a hash-map."
  [puzzle segment]
  
  ;; (dbp/dbg-puzzle (str "RSC: " (:points segment)) puzzle)

  (let [grid (:grid puzzle)
        seg-points (:points segment)
        open-points (gr/open-grid-points grid seg-points)
        cnt-open (count open-points)]
    
    (if (or 
         (not (seq open-points))         
         (< cnt-open 2)                     ; do not waste time for one place only
         (> cnt-open MAX_OPEN_COMBI))
      grid
      ;; else
      (if-let [change-grid (find-transpose-remake grid segment open-points)]
        change-grid
        {}))))

