(ns starter.browser
  (:require [reagent.dom :as rdom]
            [reagent.core :as r]
            [starter.struct :as struct]
            ))


(defonce *org-board #_[[["0" "0" "3"] ["0" "2" "0"] ["6" "0" "0"]]
                      [["9" "0" "0"] ["3" "0" "5"] ["0" "0" "1"]]
                      [["0" "0" "1"] ["8" "0" "6"] ["4" "0" "0"]]
                      [["0" "0" "8"] ["1" "0" "2"] ["9" "0" "0"]]
                      [["7" "0" "0"] ["0" "0" "0"] ["0" "0" "8"]]
                      [["0" "0" "6"] ["7" "0" "8"] ["2" "0" "0"]]
                      [["0" "0" "2"] ["6" "0" "9"] ["5" "0" "0"]]
                      [["8" "0" "0"] ["2" "0" "3"] ["0" "0" "9"]]
                      [["0" "0" "5"] ["0" "1" "0"] ["3" "0" "0"]]]
  (atom [[["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]
         [["0" "0" "0"] ["0" "0" "0"] ["0" "0" "0"]]])
  #_(atom (struct/random-board)))

(defonce *board-current (r/atom @*org-board))
(defonce *coordinate (r/atom []))

(defonce *color (r/atom ""))
(defonce *valid-nums (r/atom nil))


(defn generate-suduko
  [size]
  #_(js/console.log "board" board)
  (for [row                (vec (range 9))
        col                (vec (range 3))
        sub-col            (vec (range 3))
        :let               [x (* size row)
                            y (+ (* (* size 3) col) (* size sub-col))
                            
                            _ (if (empty? @*coordinate)
                                (if (and (or (< row 3) (> row 5)) (not= col 1))
                                  (reset! *color "lightblue")
                                  (if (and (and (> row 2) (< row 6)) (= col 1))
                                    (reset! *color "lightblue")
                                    (reset! *color "white")))
                                (if (and (= (first @*coordinate) row) (= (second @*coordinate) col) (= (last @*coordinate) sub-col))
                                  (reset! *color "red")
                                  (if (and (or (< row 3) (> row 5)) (not= col 1))
                                    (reset! *color "lightblue")
                                    (if (and (and (> row 2) (< row 6)) (= col 1))
                                      (reset! *color "lightblue")
                                      (reset! *color "white")))))
                            ]]
    [:g [:rect   {:x            x
                  :y            y
                  :width        size
                  :height       size 
                  :stroke       "black"
                  :stroke-width 2.5
                  :rx           7
                  :ry           7   
                  :onClick      (fn []
                                  (reset! *coordinate [row col sub-col])
                                  (reset! *color "red")
                                  (reset! *valid-nums nil))
                  :fill         @*color
                  }]
     [:text {:x (+ x 20)
             :y (+ y 27)
             :font-size "23px"
             :text-anchor "middle"
             :onClick (fn []
                        (reset! *coordinate [row col sub-col])
                        (reset! *color "red")
                        (reset! *valid-nums nil))
             }
      
      (if (not= "0" (get-in @*board-current [row col sub-col]))
        (get-in @*board-current [row col sub-col])
        " ")]
     ])
  )



(defn web-suduko
  [size]
  (into [:svg
         {:viewBox "-50 -30 700 600"}]
        (generate-suduko size)))




(defn input-stream
  []
  [:div 
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "1")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "1")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "1"]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "2")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "2")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "2"]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "3")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "3")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "3"]
   [:p " "]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "4")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "4")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "4"]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "5")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "5")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "5"]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "6")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "6")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "6"]
   [:p " "]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "7")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "7")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "7"]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "8")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "8")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "8"]
   [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "9")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "9")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "9"]
   [:p " "]
   [:button {:class "btn btn-outline-secondary btn-lg me-md-4 col-4"
             :onClick (fn []
                        (reset! *board-current (if (struct/insert-update-board @*org-board @*board-current @*coordinate "0")
                                                 (struct/insert-update-board @*org-board @*board-current @*coordinate "0")
                                                 @*board-current)))
             :disabled (if (empty? @*coordinate)
                         true
                         false)} "Clear"]
   [:p " "]
   [:button {:class "btn btn-danger me-md-4 col-4"
             :onClick (fn []
                        (reset! *board-current @*org-board))
             } "Reset Board!"]])


(defn home-page
  []
  (let [_ ()]
    [:div 
     [:p {:class "text-center fw-bold fs-1"} [:span {:style {:color "green"}} "Suduko"]]
     [:div {:class "row"}
      [:div {:class "col-sm-7"}
       [:div
        [:h1 "play-board"]
        #_[sudoku-board]
        [web-suduko 40]
        #_(js/console.log struct/random-board)]
       ]
      [:div {:class "col-sm"}
       [:p " "]
       [:h1 "input board"]
       [:p " "]
       [input-stream]
       #_[:h3 "coordinates are : "]
       #_[:h3 (str @*coordinate)]
       [:p " "]
       [:button {:class "button"
                 :onClick (if (empty? @*coordinate)
                            #()
                            (fn [] (reset! *valid-nums (struct/get-valid-nums @*board-current @*coordinate))))} "Hint"]
       [:h2 (str @*valid-nums)]
       [:p " "]
       [:button {:class "btn btn-info"
                 :onClick (fn []
                            (struct/generate-solved-boards @*org-board 1 10)
                            (js/console.log @struct/*solved-puzzel)
                            (reset! *board-current (first @struct/*solved-puzzel)))} "Solve this Suduko!"]
       ]]]))

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
