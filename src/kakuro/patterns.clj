(ns kakuro.patterns)

;; ----------------------------------------------------------------------


(def unique-patterns
  { [ 3 2]  #{1 2}
    [17 2] #{8 9}
    [23 3] #{6 8 9}
    [24 3] #{7 8 9}
   })

(defn get-unique-pattern [value places]
  (get unique-patterns [value places]))


