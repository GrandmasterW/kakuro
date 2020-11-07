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


(defn first-val [nilval coll]
  "Returns the first value of the collection if available, otherwise nilval"
  (if (empty? coll)
    nilval
    (first coll)))


;; debug helper
(def MAXSTEPS 10000000N)

(def steps (atom 0))

(defn reset-steps! []
  (reset! steps 0))

(defn inc-steps! []
  (swap! steps inc))

(defn get-steps []
  @steps)

(defn count-steps!? []
  (do
    (inc-steps!)
    (> (get-steps) MAXSTEPS)))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

(def D-PRINT true)

(defn log
  ([message]
   (if D-PRINT
     (println (get-steps) "\t" message)))
  ([method & args]
   (log
    (str 
     method "\t\t"
     (clojure.string/join " | " args)))))

;; ----------------------------------------------------------------------
