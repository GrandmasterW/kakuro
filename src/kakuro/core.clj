(ns kakuro.core
  (:require
   [kakuro.segment :as seg]
   [kakuro.solver :as sol]
   [kakuro.importer :as imp]
   [kakuro.logpuzzle :as lpu]
   [kakuro.strpuzzle :as stp]
   )
  (:gen-class))

(defn row [fx tx y s]
  (seg/create-row-segment fx tx y s nil))

(defn col [fy ty x s]
  (seg/create-column-segment fy ty x s nil))


(defn do-puzzle [puzzle]

  (lpu/log-start puzzle)
  (let [solutions (time (sol/start-solve puzzle))]
    (dotimes [n (count solutions)]
      (lpu/log-solution (inc n))
      (lpu/log-puzzle (assoc puzzle :grid (nth solutions n)))
      (stp/print-puzzle (assoc puzzle :grid (nth solutions n)))
        )))

(defn -main
  "print"
  [& args]

  (if-let [filename (first args)]
    (let [e-map (imp/read-puzzle-file filename)
          puzzle (imp/into-puzzle e-map)]
      (do-puzzle puzzle))
    (println "Nothing to do.")
    ))

