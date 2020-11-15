(ns kakuro.core
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.strpuzzle :as sp]
   [kakuro.util :as ut]
   [kakuro.point :as pt]
   [kakuro.segment :as seg]
   [kakuro.creation :as cr]
   [kakuro.restrict :as rst]
   [kakuro.solver :as sol]
   [kakuro.importer :as imp]
   )
  (:gen-class))

(defn row [fx tx y s]
  (seg/create-row-segment fx tx y s nil))

(defn col [fy ty x s]
  (seg/create-column-segment fy ty x s nil))


(defn do-puzzle [puzzle]
  (println "------------------ Start --------------------------------")

  (let [r-puzzle (rst/restrict-values puzzle)]
    (sp/print-puzzle-stats r-puzzle)
    (sp/print-puzzle r-puzzle)
    )
  
  (let [solutions (time (sol/start-solve puzzle))]
    ;;    (shutdown-agents)
    (println "--------------------------------------------------")
    (dotimes [n (count solutions)]
      (println "Solution #" n)
      (sp/print-puzzle (assoc puzzle :grid (nth solutions n)))))
  (println "-------------------End   -------------------------------"))

(defn -main
  "print"
  [& args]

  (if-let [filename (first args)]
    (let [e-map (imp/read-puzzle-file filename)
          puzzle (imp/into-puzzle e-map)]
      (do-puzzle puzzle))))

