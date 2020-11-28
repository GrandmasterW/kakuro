(ns kakuro.patterns
  (:require
   [clojure.math.combinatorics :as combo]
   [clojure.set :as cs]
))

;; ----------------------------------------------------------------------


;; key: [value places] val: set of digits
(def unique-patterns 
{[3 2] #{1 2},
 [4 2] #{1 3},
 [6 3] #{1 3 2},
 [7 3] #{1 4 2},
 [10 4] #{1 4 3 2},
 [11 4] #{1 3 2 5},
 [15 5] #{1 4 3 2 5},
 [16 2] #{7 9},
 [16 5] #{1 4 6 3 2},
 [17 2] #{9 8},
 [21 6] #{1 4 6 3 2 5},
 [22 6] #{7 1 4 3 2 5},
 [23 3] #{6 9 8},
 [24 3] #{7 9 8},
 [28 7] #{7 1 4 6 3 2 5},
 [29 4] #{7 9 5 8},
 [29 7] #{1 4 6 3 2 5 8},
 [30 4] #{7 6 9 8},
 [34 5] #{7 4 6 9 8},
 [35 5] #{7 6 9 5 8},
 [36 8] #{7 1 4 6 3 2 5 8},
 [37 8] #{7 1 4 6 3 2 9 5},
 [38 6] #{7 6 3 9 5 8},
 [38 8] #{1 4 6 3 2 9 5 8},
 [39 6] #{7 4 6 9 5 8},
 [39 8] #{7 1 4 3 2 9 5 8},
 [40 8] #{7 1 4 6 3 2 9 8},
 [41 7] #{7 4 6 2 9 5 8},
 [41 8] #{7 1 6 3 2 9 5 8},
 [42 7] #{7 4 6 3 9 5 8},
 [42 8] #{7 1 4 6 2 9 5 8},
 [43 8] #{7 1 4 6 3 9 5 8},
 [44 8] #{7 4 6 3 2 9 5 8},
 ;; pointless
 ;; [45 9] #{7 1 4 6 3 2 9 5 8}
 })

(def all-patterns
  {[3 2] #{1 2},
   [4 2] #{1 3},
   [5 2] #{1 4 3 2},
   [6 2] #{1 4 2 5},
   [6 3] #{1 3 2},
   [7 2] #{1 4 6 3 2 5},
   [7 3] #{1 4 2},
   [8 2] #{7 1 6 3 2 5},
   [8 3] #{1 4 3 2 5},
   [9 2] #{7 1 4 6 3 2 5 8},
   [9 3] #{1 4 6 3 2 5},
   [10 2] #{7 1 4 6 3 2 9 8},
   [10 3] #{7 1 4 6 3 2 5},
   [10 4] #{1 4 3 2},
   [11 2] #{7 4 6 3 2 9 5 8},
   [11 3] #{7 1 4 6 3 2 5 8},
   [11 4] #{1 3 2 5},
   [12 2] #{7 4 3 9 5 8},
;;   [12 3] #{7 1 4 6 3 2 9 5 8},
   [12 4] #{1 4 6 3 2 5},
   [13 2] #{7 4 6 9 5 8},
;;   [13 3] #{7 1 4 6 3 2 9 5 8},
   [13 4] #{7 1 4 6 3 2 5},
   [14 2] #{6 9 5 8},
;;   [14 3] #{7 1 4 6 3 2 9 5 8},
   [14 4] #{7 1 4 6 3 2 5 8},
   [15 2] #{7 6 9 8},
;;   [15 3] #{7 1 4 6 3 2 9 5 8},
;;   [15 4] #{7 1 4 6 3 2 9 5 8},
   [15 5] #{1 4 3 2 5},
   [16 2] #{7 9},
;;   [16 3] #{7 1 4 6 3 2 9 5 8},
;;   [16 4] #{7 1 4 6 3 2 9 5 8},
   [16 5] #{1 4 6 3 2},
   [17 2] #{9 8},
;;   [17 3] #{7 1 4 6 3 2 9 5 8},
;;   [17 4] #{7 1 4 6 3 2 9 5 8},
   [17 5] #{7 1 4 6 3 2 5},
;;   [18 3] #{7 1 4 6 3 2 9 5 8},
;;   [18 4] #{7 1 4 6 3 2 9 5 8},
   [18 5] #{7 1 4 6 3 2 5 8},
   [19 3] #{7 4 6 3 2 9 5 8},
;;   [19 4] #{7 1 4 6 3 2 9 5 8},
;;   [19 5] #{7 1 4 6 3 2 9 5 8},
   [20 3] #{7 4 6 3 9 5 8},
;;   [20 4] #{7 1 4 6 3 2 9 5 8},
;;   [20 5] #{7 1 4 6 3 2 9 5 8},
   [21 3] #{7 4 6 9 5 8},
;;   [21 4] #{7 1 4 6 3 2 9 5 8},
;;   [21 5] #{7 1 4 6 3 2 9 5 8},
   [21 6] #{1 4 6 3 2 5},
   [22 3] #{7 6 9 5 8},
;;   [22 4] #{7 1 4 6 3 2 9 5 8},
;;   [22 5] #{7 1 4 6 3 2 9 5 8},
   [22 6] #{7 1 4 3 2 5},
   [23 3] #{6 9 8},
;;   [23 4] #{7 1 4 6 3 2 9 5 8},
;;   [23 5] #{7 1 4 6 3 2 9 5 8},
   [23 6] #{7 1 4 6 3 2 5 8},
   [24 3] #{7 9 8},
;;   [24 4] #{7 1 4 6 3 2 9 5 8},
;;   [24 5] #{7 1 4 6 3 2 9 5 8},
;;   [24 6] #{7 1 4 6 3 2 9 5 8},
;;   [25 4] #{7 1 4 6 3 2 9 5 8},
;;   [25 5] #{7 1 4 6 3 2 9 5 8},
;;   [25 6] #{7 1 4 6 3 2 9 5 8},
   [26 4] #{7 4 6 3 2 9 5 8},
;;   [26 5] #{7 1 4 6 3 2 9 5 8},
;;   [26 6] #{7 1 4 6 3 2 9 5 8},
   [27 4] #{7 4 6 3 9 5 8},
;;   [27 5] #{7 1 4 6 3 2 9 5 8},
;;   [27 6] #{7 1 4 6 3 2 9 5 8},
   [28 4] #{7 4 6 9 5 8},
;;   [28 5] #{7 1 4 6 3 2 9 5 8},
;;   [28 6] #{7 1 4 6 3 2 9 5 8},
   [28 7] #{7 1 4 6 3 2 5},
   [29 4] #{7 9 5 8},
;;   [29 5] #{7 1 4 6 3 2 9 5 8},
;;   [29 6] #{7 1 4 6 3 2 9 5 8},
   [29 7] #{1 4 6 3 2 5 8},
   [30 4] #{7 6 9 8},
;;   [30 5] #{7 1 4 6 3 2 9 5 8},
;;   [30 6] #{7 1 4 6 3 2 9 5 8},
;;   [30 7] #{7 1 4 6 3 2 9 5 8},
;;   [31 5] #{7 1 4 6 3 2 9 5 8},
;;   [31 6] #{7 1 4 6 3 2 9 5 8},
;;   [31 7] #{7 1 4 6 3 2 9 5 8},
   [32 5] #{7 4 6 3 2 9 5 8},
;;   [32 6] #{7 1 4 6 3 2 9 5 8},
;;   [32 7] #{7 1 4 6 3 2 9 5 8},
   [33 5] #{7 4 6 3 9 5 8},
;;   [33 6] #{7 1 4 6 3 2 9 5 8},
;;   [33 7] #{7 1 4 6 3 2 9 5 8},
   [34 5] #{7 4 6 9 8},
;;   [34 6] #{7 1 4 6 3 2 9 5 8},
;;   [34 7] #{7 1 4 6 3 2 9 5 8},
   [35 5] #{7 6 9 5 8},
;;   [35 6] #{7 1 4 6 3 2 9 5 8},
;;   [35 7] #{7 1 4 6 3 2 9 5 8},
;;   [36 6] #{7 1 4 6 3 2 9 5 8},
;;   [36 7] #{7 1 4 6 3 2 9 5 8},
   [36 8] #{7 1 4 6 3 2 5 8},
   [37 6] #{7 4 6 3 2 9 5 8},
;;   [37 7] #{7 1 4 6 3 2 9 5 8},
   [37 8] #{7 1 4 6 3 2 9 5},
   [38 6] #{7 6 3 9 5 8},
;;   [38 7] #{7 1 4 6 3 2 9 5 8},
   [38 8] #{1 4 6 3 2 9 5 8},
   [39 6] #{7 4 6 9 5 8},
;;   [39 7] #{7 1 4 6 3 2 9 5 8},
   [39 8] #{7 1 4 3 2 9 5 8},
;;   [40 7] #{7 1 4 6 3 2 9 5 8},
   [40 8] #{7 1 4 6 3 2 9 8},
   [41 7] #{7 4 6 2 9 5 8},
   [41 8] #{7 1 6 3 2 9 5 8},
   [42 7] #{7 4 6 3 9 5 8},
   [42 8] #{7 1 4 6 2 9 5 8},
   [43 8] #{7 1 4 6 3 9 5 8},
   [44 8] #{7 4 6 3 2 9 5 8}
;;   [45 9] #{7 1 4 6 3 2 9 5 8}
   })



(defn get-pattern
  "returns a unique pattern for sum value in places from the internal map"
  [value places]
  (get all-patterns [value places]))


(defn comp-patterns
  [value places digits]
  (mapv
   #(hash-map [value places] (into #{} %1))
   (filter
    #(= value (reduce + %1))
    (combo/combinations digits places))))

(defn comp-all-patterns
  "compute patterns - non unique. digits should by a set." 
  [maxval maxplaces digits]
  {:pre (set? digits)}

  ;; compute the combinations of values to places in a nested loop and remove nils and empty
  (reduce concat []
          (->> (for [v (range 3 (inc maxval))
                     p (range 2 (inc maxplaces)) ]
                 (comp-patterns v p digits))
               (remove nil?)
               (remove empty?))))



(defn comp-unique-patterns
  "compute unique patterns for values till maxval and places till maxplaces"
  ([]
   ;; helper to start
   (comp-unique-patterns 45 9 (range 1 10)))
  ([maxval maxplaces digits]
   {:pre (set? digits)}
   (let [
         ;; compute the combinations of values to places in a nested loop and remove nils and empty
         val-res (comp-all-patterns maxval maxplaces digits)
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


(defn fm [col [k v]]
  (update col k (partial cs/union v)))

(defn comp-joined-patterns
  "computes the patterns till maxval and for maxplaces for digits, but not unqiue:
   returns for each key the merged i.e. per union integrated set of values, if less
   than 9 values. Having values in range 1 to 9 is pointless..."
  [maxval maxplaces digits ]
  {:pre [(> maxval 0)
         (> maxplaces 0)
         (set? digits)]}
   (let [
         ;; compute the combinations of values to places in a nested loop and remove nils and empty
         val-res (comp-all-patterns maxval maxplaces digits)
         ps (reduce fm {} (map first val-res))
         ]
     (into (sorted-map) ps)))
