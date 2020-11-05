(ns kakuro.patterns
  (:require [clojure.math.combinatorics :as combo]))

;; ----------------------------------------------------------------------


;; key: [value places] val: set of digits
(def unique-patterns 
{[17 2] #{9 8},
 [16 5] #{1 4 6 3 2},
 [6 3] #{1 3 2},
 [7 3] #{1 4 2},
 [4 2] #{1 3},
 [34 5] #{7 4 6 9 8},
 [37 8] #{7 1 4 6 3 2 9 5},
 [16 2] #{7 9},
 [29 7] #{1 4 6 3 2 5 8},
 [41 8] #{7 1 6 3 2 9 5 8},
 [24 3] #{7 9 8},
 [38 6] #{7 6 3 9 5 8},
 [40 8] #{7 1 4 6 3 2 9 8},
 [39 8] #{7 1 4 3 2 9 5 8},
 [21 6] #{1 4 6 3 2 5},
 [11 4] #{1 3 2 5},
 [29 4] #{7 9 5 8},
 [30 4] #{7 6 9 8},
 [15 5] #{1 4 3 2 5},
 [39 6] #{7 4 6 9 5 8},
 [28 7] #{7 1 4 6 3 2 5},
 [41 7] #{7 4 6 2 9 5 8},
 [35 5] #{7 6 9 5 8},
 [42 8] #{7 1 4 6 2 9 5 8},
 [10 4] #{1 4 3 2},
 [38 8] #{1 4 6 3 2 9 5 8},
 [42 7] #{7 4 6 3 9 5 8},
 [43 8] #{7 1 4 6 3 9 5 8},
 [45 9] #{7 1 4 6 3 2 9 5 8},
 [22 6] #{7 1 4 3 2 5},
 [23 3] #{6 9 8},
 [44 8] #{7 4 6 3 2 9 5 8},
 [3 2] #{1 2},
 [36 8] #{7 1 4 6 3 2 5 8}
 })

(defn get-unique-pattern
  "returns a unique pattern for sum value in places from the internal map"
  [value places]
  (get unique-patterns [value places]))

(defn comp-patterns
  [value places digits]
  (mapv
   #(hash-map [value places] (into #{} %1))
   (filter
    #(= value (reduce + %1))
    (combo/combinations digits places))))

(defn comp-unique-patterns
  "compute unique patterns for values till maxval and places till maxplaces"
  ([]
   ;; helper to start
   (comp-unique-patterns 45 9 (range 1 10)))
  ([maxval maxplaces digits]
   (let [
         ;; compute the combinations of values to places in a nested loop and remove nils and empty
         val-res
         (reduce concat []
                 (->> (for [v (range 3 (inc maxval))
                            p (range 2 (inc maxplaces)) ]
                        (comp-patterns v p digits))
                      (remove nil?)
                      (remove empty?)))
         ;; Prepare frequencies as we only need unique patterns
         freq (frequencies (map (comp first first) val-res))
         ;; extract the keys [val places] with frequency 1 into a set 
         unique-key-set (into #{}
                              (mapv first
                                    (filter (comp (partial = 1) second) freq)))  ]
     ;; get the key-val-entries from the value-set for each key, i.e. for each
     ;;  key in val-res, only keep the entry if available in unique-keys,
     ;;  reduce into a single hash-map
     (reduce merge {} (filter (comp unique-key-set first first) val-res)))))
