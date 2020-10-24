(ns kakuro.core
  (:require [kakuro.puzzle :as pu]
            [kakuro.util :as ut]
;;            [kakuro.cell :as ce]
            [kakuro.point :as pt]
            [kakuro.segment :as seg]
            [kakuro.creation :as cr]
            )
  (:gen-class))


;; real example from kakuro
(def segments1
  [(seg/create-row-segment 2 3 1 16 nil)
   (seg/create-row-segment 1 3 2 15 nil)
   (seg/create-row-segment 1 3 3 19 nil)
   ;; columns
   (seg/create-column-segment 2 3 1 17 nil)
   (seg/create-column-segment 1 3 2 23 nil)
   (seg/create-column-segment 1 3 3 10 nil)
   ])

(def puzzle1 (cr/create-puzzle segments1))

(def puzzle1 (assoc-in puzzle1 [:grid (pt/->Point 1 2)] #{1}))

;; simple example
(def segments2x2 [(seg/create-row-segment 1 2 1 3 nil)
                  (seg/create-row-segment 1 2 2 7 nil)
                  (seg/create-column-segment 1 2 1 4 nil)
                  (seg/create-column-segment 1 2 1 6 nil)])

(def puzzle2 (cr/create-puzzle segments2x2))

(defn -main
  "print"
  [& args]
  (pu/print-puzzle-stats puzzle1)
  (println "--------------------------------------------------")
  ;;  (pu/print-puzzle-stats puzzle2)
  (pu/print-puzzle puzzle1)
  )
