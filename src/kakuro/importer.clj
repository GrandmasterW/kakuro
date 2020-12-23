(ns kakuro.importer
  (:require
   [kakuro.puzzle :as pu]
   [kakuro.point :as pt]
   [kakuro.creation :as cr]
   [kakuro.segment :as seg]
   [clojure.edn :as edn]
   [clojure.string :as cst]
   )
  )

(comment
"Utilities to import puzzles from files by reading edn 
 documents"
)

(defn- read-puzzle-file
  "Reads an edn file containing a hash-map with keys :rows for row-vectors, :columns for
  column vectors and :solutions for a vector containing hash-maps of point to value grids"
  [filename]
  (-> filename
      slurp ; a string
      edn/read-string ; a hash-map
      )  )

(defn- create-seg [orientation [from-a to-a b sum]]
  (seg/new-segment orientation from-a to-a b sum))


;; ----------------------------------------------------------------------
(def whitespaces "characters to be ignored while reading" #{\space \|})
(def placeholders "characters serving as placeholders for cells" #{\- \_})

(defn char-to-int
  "converts digit char into int. Only useful for digits, though not ensured."
  [c]
  (- (int c)(int \0)))

(defn make-grid-cell
  "Returns a hash-map with one entry: point to #{val}"
  [x y val]
  {(pt/Point x y) #{val} })

(def merger "function to reduce with merge" (partial reduce merge {}))

(defn convert-row-str
  "Takes a string in form '- - - 4 5 - 6 7 - ' and
   converts it into a hash-map of points with digit value sets." 
  [row-id row-str]
  (let [cells
        (->> row-str
             (seq)
             (remove #(contains? whitespaces %1))
             (mapv list (range))
             (remove (comp (partial contains? placeholders) second))
             (mapv #(make-grid-cell
                     (inc (first %1))
                     row-id
                     (char-to-int (second %1))))
             (merger)
             )
        ]
    cells))

;;(convert-row-str 1 "- - 3 4 - 5 6")

(defn convert-solution-str
  "Reads a multi-line string, each line represents a row of the solution and should look
   like: - - - 3 4 - 5 6 -
   Hence it consists of placeholders, here: -, and digits.
   For a solution string we compute the grid matching the points and values (value sets
   with one digit)."
  [solution]
  (let [rows
        (->> solution
             (cst/split-lines)
             (mapv #(convert-row-str (inc %1) %2) (range))
             (merger))
        ]
    rows
    ))

(comment

  (convert-solution-str
 "- - 1 2
  1 2 3 4
  - 5 6 - 
  7 8 9 -")

  )

(defn into-puzzle
  "converts a hash-map containing :rows, :columns, and :solutions into a valid puzzle"
  [e-map]
  (let [rows (mapv (partial create-seg :h) (:rows e-map))
        cols (mapv (partial create-seg :v) (:columns e-map))
        solutions (mapv convert-solution-str (:solutions e-map))
        puzzle     (cr/create-puzzle (into [] (concat rows cols)))
        ]
    (assoc puzzle :solutions solutions)))

(defn read-puzzle
  "Reads the given file and returns a puzzle structure"
  [filename]
  (-> filename
      read-puzzle-file
      into-puzzle
      pu/assert-points-match))


