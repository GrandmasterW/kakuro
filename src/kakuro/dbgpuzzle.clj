(ns kakuro.dbgpuzzle
  (:require
   [kakuro.util :as util]
   [kakuro.strpuzzle :as stp]
            ))

(defn dbg-puzzle
  ([puzzle]
     (println )
     (util/log (stp/puzzle-to-str puzzle)) )
  ([msg puzzle]
   (println )
   (util/log (str msg "\n") (stp/puzzle-to-str puzzle)))
  )
