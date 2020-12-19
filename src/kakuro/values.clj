(ns kakuro.values)

(defn values-unique? 
  "true if each set contains only one digit, and all values of all sets appear only once"
  [value-sets]
  (and (every? (comp (partial = 1) count) value-sets)
       (every? (partial = 1) (vals (frequencies (mapv first value-sets))))))

(defn get-value-sets
  "Returns a collection of value-sets from grid for the given points"
  [grid points]
  (mapv (partial get grid) points))

