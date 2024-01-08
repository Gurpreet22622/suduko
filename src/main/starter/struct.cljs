(ns starter.struct
  (:require [clojure.pprint :as pp]
            [clojure.string :as str]
            ))

(defn generate-board 
  [inp]
  (let [parts (str/split inp #"\n")
        sub-part (mapv #(vec (mapv vec (re-seq #".{1,3}" %))) parts)]
    sub-part))

(defn filter-unique
  [lst]
  ;;takes a vector of vector and return a sequence with no zero occuring in data
  (remove zero? (mapv #(apply int %) (vec (flatten lst)))))

(defn check-board-row
  [board]
  (let [test-vec (filter-unique (first board))
        unique? (if (empty? test-vec)
                  true
                  (apply distinct? test-vec))]
    #_(pp/pprint test-vec)
    (case unique?
          true (if (= (count board) 1)
                 true
                 (check-board-row (vec (rest board))))
          false false)))


(defn check-board-column
  [board]
  (let [flatten-lst (mapv #(vec (flatten %)) board)
        trp-lst (vec (apply map vector flatten-lst))
        trp-brd (mapv #(mapv vec (partition 3 %)) trp-lst)]
    #_(pp/pprint trp-brd)
    (check-board-row trp-brd)))


(defn check-board-sector
  [board]
  (let [sec1 [(get-in board [0 0]) (get-in board [1 0]) (get-in board [2 0])]
        chk1 (if (empty? (filter-unique sec1))
               true
               (apply distinct? (filter-unique sec1)))
        sec2 [(get-in board [0 1]) (get-in board [1 1]) (get-in board [2 1])]
        chk2 (if (empty? (filter-unique sec2))
               true
               (apply distinct? (filter-unique sec2)))
        sec3 [(get-in board [0 2]) (get-in board [1 2]) (get-in board [2 2])]
        chk3 (if (empty? (filter-unique sec3))
               true
               (apply distinct? (filter-unique sec3)))] 
    (cond
      (and chk1 chk2 chk3 (= (count board) 3)) true
      (and chk1 chk2 chk3) (check-board-sector (vec (drop 3 board)))
      :else false)))


(defn validate-board
  [board]
  (let [chk1 (check-board-row board)
        chk2 (check-board-column board)
        chk3 (check-board-sector board)]
    (and chk1 chk2 chk3)))





(comment
  
  (let [board1 (generate-board "003020600
900305001
001806400
008102900
700000008
006708200
002609500
800203009
005010300")
        ]
    (validate-board board1))
  )