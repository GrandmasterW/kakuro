(ns kakuro.constraints
  (:require [kakuro.util :as util]))

;;
;; definition and application of constraints to cells
;; 

(defn create-constraint [orientation from-a to-a b max-value]
  "creates a vector containing the orientation as :row or :column, from-a, to-a and max-value"
  [orientation from-a to-a max-value]) ;; to do apply spec or checs or define a type?

(defn apply-constraints [cells constraints]
  "reduces the potential values of each cell to the possible maximum by applying the max-value to each cell
   and to each row/col segments sum"
  ;; to do
  cells
  )
