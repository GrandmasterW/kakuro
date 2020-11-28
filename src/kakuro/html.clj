(ns kakuro.html
  (:require
   [kakuro.point :as pt]
   [kakuro.puzzle :as pu]
   [kakuro.strpuzzle :as stp]
   [kakuro.util :as util]
   [hiccup.core :refer [html]]
   [hiccup.page :refer [include-css]] ))

;; ----------------------------------------------------------------------
;; HTML support

(defn segments-html
  "create a table for segements data"
  [segments]
  (html
   [:h2 "Segments"]
   [:table 
    [:thead
     [:tr
      [:th "Orientation"]
      [:th "from-a"]
      [:th "to-a"]
      [:th "b"]
      [:th "sum"]
      ]
     ]
    [:tbody
     (for [s segments]
       [:tr
        [:td (:orientation s)]
        [:td (:from-a s)]
        [:td (:to-a s)]
        [:td (:b s)]
        [:td (:sum s)]
        ])
       ]
    ]))

(defn puzzle-stats-html 
  "Helper to print"
  [puzzle]
  (let [grid (:grid puzzle)
        points (keys grid)
        dimensions (pu/puzzle-dimensions puzzle)]
    (html
     [:h2 "Puzzle data"]
     [:p (str "Cells: " (count points))]
     [:p (str "Max x: " (first dimensions))]
     [:p (str "Max y: " (second dimensions))]
     [:p (str "Combinations: " (pu/count-potential-solutions puzzle))]
     (segments-html (:segments puzzle)))))

(defn puzzle-html
  "Return a table made from puzzle grid"
  [puzzle]
  (let [grid (:grid puzzle)
        dimensions (pu/puzzle-dimensions puzzle)
        x-max (first dimensions)
        y-max (second dimensions)]
    (html [:table

           [:thead
            [:tr
             (for [x (util/fullrange 0 x-max)]
               [:th (:style "border: 1;") x])
             ]
            ]

           [:tbody
            (for [y (util/fullrange 1 y-max)]
              [:tr
               (for [x (util/fullrange 0 x-max)]
                 [:td
                  (if (= x 0)
                    y
                    (stp/values-str (grid (pt/Point x y))))])
               ])
            ]
           ])))


(defn html-start
  []
  (html [:head
         [:link (include-css "t.css")]
         ]
  ))


