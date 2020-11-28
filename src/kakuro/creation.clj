(ns kakuro.creation
  (:require [kakuro.point :as pt]
            ;;[kakuro.segment :as seg]
            [kakuro.grid :as grid]
            [kakuro.util :as util]
            [kakuro.puzzle :as puzzle]
            ))

;; ----------------------------------------------------------------------
;; Collection of creation helpers to set up a puzzle and its parts
;;
;; ----------------------------------------------------------------------
(defn create-points-for-segment
  "returns a set of freshly created points defined by the segment"
   [segment]
  (let [type (:orientation segment)
        from-a (:from-a segment)
        to-a (:to-a segment)
        b (:b segment)
        pcf (case type
              :h #(pt/Point %1 b)
              :v #(pt/Point b %1))]
    (mapv pcf (range from-a (inc to-a)))))

(defn create-all-points
  "creates all points over the segments, unifying them by using sets. Returns a set."
   [segments]
  (into #{} (mapcat create-points-for-segment segments)))

;;
;; assign points
;;
(defn assign-points-to-segment 
  "returns a segment, holding a set of points it references to. It overrides existing points!"
  [new-points segment]
  (let [orientation (:orientation segment)
        from-a (:from-a segment)
        to-a (:to-a segment)
        b (:b segment)
        coord (case orientation
                :h :x
                :v :y)
        s-points (into #{} (filter #(pt/point-between? %1 coord from-a to-a b) new-points))]
    (assoc segment :points s-points)))


(defn assign-all-segments 
  "iterates over all segments, assigning points to each, returning a vector of updated segments"
  [new-points segments]
  (mapv (partial assign-points-to-segment new-points) segments))

(defn create-initial-value-set 
  "creates a set containing the values from min to max incl."
  [vmin vmax]
  (into #{} (util/fullrange vmin vmax)))

(defn make-initial-grid-for-segments 
  "creates the points and the initial range for values for each, returns a hashmap point->cell"
  [segments vmin vmax]
  (let [points (create-all-points segments)
        vrange (create-initial-value-set vmin vmax)]
    (into {} (mapcat hash-map points (repeat vrange)))))

(defn create-puzzle
   "creates a puzzle as cells from the row segments in segments and from the segments, using the full range of min and max values. If invoked without vmin and vmax, use 1 and 9."
  ([segments]
   (create-puzzle segments 1 9))
  ([segments vmin vmax]
   (let [mygrid (make-initial-grid-for-segments segments vmin vmax)
         full-segments (assign-all-segments (grid/get-points-from-grid mygrid) segments)]
   (puzzle/->Puzzle mygrid full-segments vmin vmax))))
