(ns kakuro.importer
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.strpuzzle :as spz]
   [kakuro.util :as util]
   [kakuro.point :as pt]
   [kakuro.grid :as gr]
   [kakuro.creation :as cr]
   [kakuro.segment :as seg]
   [kakuro.restrict :as rst]
   [clojure.set :as cs]
   [clojure.edn :as edn]
   )
  )

(comment
"Utilities to import puzzles from files by reading edn 
 documents"
)

(defn read-puzzle-file
  "Reads an edn file containing a hash-map with keys :rows for row-vectors, :columns for
  column vectors and :solutions for a vector containing hash-maps of point to value grids"
  [filename]
  (-> filename
      slurp ; a string
      edn/read-string ; a hash-map
      )  )

(defn create-seg [orientation [from-a to-a b sum]]
  (seg/new-segment orientation from-a to-a b sum))

(defn into-puzzle
  "converts a hash-map containing :rows and :columns into a valid puzzle"
  [e-map]
  (let [rows (mapv (partial create-seg :h) (:rows e-map))
        cols (mapv (partial create-seg :v) (:columns e-map)) ]
    (cr/create-puzzle (into [] (concat rows cols)))))


        
