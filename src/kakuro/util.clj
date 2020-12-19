(ns kakuro.util
  (:require [clojure.string :as s]))



;; math helpers
(defn transpose [m]
  {:pre [(seq m)]}
  (apply mapv vector m))


(defn betweenl 
  "true, if from <= x <= to for integers"
  [x from to]
  (and (>= x from) (<= x to)))

(defn betweenr
  "changes order or parameters to allow comp, returns between x from to"
   [from to x]
  (betweenl x from to))

(defn fullrange
  "returns a range from from to to inclusive, i.e. (inc to), step 1"
  ([from to] 
   (fullrange from to 1))
  ([from to step] 
   (if (> step 0)
     (range from (inc to) step)
     (range from (dec to) step))))


(defn first-val 
  "Returns the first value of the collection if available, otherwise nilval"
  [nilval coll]
  (if (empty? coll)
    nilval
    (first coll)))

;; ----------------------------------------------------------------------
;; parallel execution helpers - not used any longer... 
;;
(def par-switch (atom 0))
(def max-par 0)

(defn get-mapper []
  (if (< @par-switch max-par)
    (do
      (swap! par-switch inc)
      pmap)
    map))


;; debug helper
;;            ...,,,...  
(def MAXSTEPS 900000000N)

(def steps (atom 0N))

(defn reset-steps! []
  (reset! steps 0))

(defn inc-steps! []
  (swap! steps inc))

(defn get-steps []
  @steps)

(defn count-steps!? []
  (inc-steps!)
  (> (get-steps) MAXSTEPS))

;; ----------------------------------------------------------------------
;; ----------------------------------------------------------------------

(def D-PRINT true)

(defn log
  ([message]
   (if D-PRINT
     (println (str "#" (get-steps)) "\t" message)
     nil))
  ([method & args]
   (log
    (str 
     method "\n"
     (s/join " | " args)
     "\n"))))

;; ----------------------------------------------------------------------
