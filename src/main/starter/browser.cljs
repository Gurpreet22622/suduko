(ns starter.browser
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [starter.struct :as struct]
            ))

(defn generate-suduko
  [board size]
  #_(js/console.log "board" board)
  (for [row                (vec (range 9))
        col                (vec (range 3))
        sub-col            (vec (range 3))
        :let               [x (* size row)
                            y (+ (* (* size 3) col) (* size sub-col))
                            ]]
    [:g [:rect   {:x            x
                  :y            y
                  :width        size
                  :height       size
                  :fill         (if (and (or (< row 3) (> row 5)) (not= col 1))
                                  "lightblue"
                                  (if (and (and (> row 2) (< row 6)) (= col 1))
                                    "lightblue"
                                    "white"))
                  :stroke       "black"
                  :stroke-width 2.5
                  :rx           7
                  :ry           7             
                  }]
     [:text {:x (+ x 15)
             :y (+ y 22)
             :font-size "20px"
             :text-anchor "middle"
             }
      #_(get-in board [row col sub-col])
      (if (not= "0" (get-in board [row col sub-col]))
        (get-in board [row col sub-col])
        " ")]
     #_[:button {:type "button"
               :onClick #()} "hi"]])
  )



(defn web-suduko
  [board size]
  (into [:svg
         {:viewBox "-50 -30 700 600"}]
        (generate-suduko board size)))





(defn home-page
  []
  (let [board-current (struct/random-board)]
    (js/console.log board-current)
    [:div
     [:h1 "Suduko"]
     #_[sudoku-board]
     [web-suduko board-current 30]
     #_(js/console.log struct/random-board)]))

(comment 
  (struct/random-board)
  )

(defn init []
  [:div
   [home-page]
   
   ])




(defn start []
  (rdom/render [init] (js/document.getElementById "app")))

(start)
