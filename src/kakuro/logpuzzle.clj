(ns kakuro.logpuzzle
  (:require
   [kakuro.html :as h]
   [kakuro.util :as util]
   [hiccup.core :refer [html]]
   ))


(defn log-puzzle
  ([puzzle]
   (println (html
              (h/puzzle-html puzzle)
              [:br])))
  ([msg puzzle]
   (println (html
             [:p msg]
              (h/puzzle-html puzzle)
              [:br])))
  ([msg trail puzzle]
   (println (html
             [:p msg " " trail]
              (h/puzzle-html puzzle)
              [:br]))))


(defn log-step-trail
  [trail]
  (println (html [:h3 (str "Step " (util/get-steps))]))
  (println (html [:p (str "Trail: " trail)])))

(defn log-steps-solutions [steps solutions]
  (println
   (html [:h1 "Results"]
         [:p (str "Steps: " steps)]
         [:p (str "Solutions: " solutions)]
          )))

(defn log-start [puzzle]
  (println (h/html-start))
  (println (html [:h1 "Start"]))
  (println (h/puzzle-stats-html puzzle))
  (println (html [:h2 "Start puzzle"]))
  (log-puzzle puzzle)
  (println (html [:h1 "Data"]))
  )

(defn log-solution
  [n]
  (println
   (html
    [:h2 (str "Solution # " n)]
    )))

