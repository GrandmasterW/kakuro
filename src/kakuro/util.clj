(ns kakuro.util)
;; math helpers

(defn betweenl [x from to]
  "true, if from <= x <= to for integers"
  (and (>= x from) (<= x to)))

(defn betweenr [from to x]
  "changes order or parameters to allow comp, returns between x from to"
  (betweenl x from to))

(defn fullrange
  ([from to] "returns a range from from to to inclusive, i.e. (inc to), step 1"
   (fullrange from to 1))
  ([from to step] "returns a range from from to to inclusive, but with stepwidth step. If step < 0, then to is decreased by 1"
   (if (> step 0)
     (range from (inc to) step)
     (range from (dec to) step))))


(def D-PRINT true)

(defn log [str]
  (if D-PRINT (println str)))

