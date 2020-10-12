(ns kakuro.core
  (:require [kakuro.puzzle :as pu]
            [kakuro.util :as ut]
            [kakuro.cell :as ce]
            [kakuro.point :as pt]
            [kakuro.segment :as seg]
            )
            
  (:gen-class))


;; real example from kakuro
(def segments1
  [(seg/row-segment 2 3 1 16)
   (seg/row-segment 1 3 2 15)
   (seg/row-segment 1 3 3 19)
   ;; columns
   (seg/column-segment 2 3 1 17)
   (seg/column-segment 1 3 2 23)
   (seg/column-segment 1 3 3 10)
   ])

(def puzzle1 (pu/create-puzzle segments1 1 9))


;; simple example
(def segments2x2 [(seg/row-segment 1 2 1 3)
                  (seg/row-segment 1 2 2 7)
                  (seg/column-segment 1 2 1 4)
                  (seg/column-segment 1 2 1 6)])

(def puzzle2 (pu/create-puzzle segments2x2 1 9))

(defn -main
  "print"
  [& args]
  (pu/print-puzzle puzzle2))
