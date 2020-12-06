(ns kakuro.restrict.combis
  (:require
   [kakuro.util :as util]
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

