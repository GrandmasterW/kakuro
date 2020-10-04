(ns kakuro.util)
;; math helpers

(defn between [x from to]
  "true, if from <= x <= to for integers"
  (and (>= x from) (<= x to)))


(def D-PRINT true)

(defn log [str]
  (if D-PRINT (println str)))

