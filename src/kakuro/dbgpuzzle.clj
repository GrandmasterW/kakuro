(ns kakuro.dbgpuzzle
  (:require
   [kakuro.util :as util]
   [kakuro.strpuzzle :as stp]
            ))

(defn dbg-puzzle
  ([puzzle]
     (println )
     (println (stp/puzzle-to-str puzzle)) )
  ([msg puzzle]
   (println )
   (println (str msg "\n") (stp/puzzle-to-str puzzle)))
  )

(def x-sorter (comp first key))
(def y-sorter (comp second key))

(defn grid-sort [grid]
  (sort-by y-sorter (sort-by x-sorter grid)))

(defn dbg-grid
  ([grid]
     (println )
     (println (grid-sort grid)))
  ([msg grid]
   (println )
   (print msg)
   (dbg-grid grid))
  )
