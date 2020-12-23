(ns kakuro.dbgpuzzle
  (:require
   [kakuro.strpuzzle :as stp]
   ))

(defn dbg-puzzle
  ([puzzle]
     (println "<br/>")
     (println (stp/puzzle-to-str puzzle)) "<br/>")
  ([msg puzzle]
   (println "<br/>")
   (println (str msg "<br/>\n") (stp/puzzle-to-str puzzle)) "<br/>")
  )

(def x-sorter (comp first key))
(def y-sorter (comp second key))

(defn grid-sort [grid]
  (sort-by y-sorter (sort-by x-sorter grid)))

(defn dbg-grid
  ([grid]
     (println "<br/>")
     (println (grid-sort grid) "<br/>"))
  ([msg grid]
   (println "<br/>")
   (print msg )
   (dbg-grid grid))
  )
