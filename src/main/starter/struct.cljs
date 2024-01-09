(ns starter.struct
  (:require   [clojure.string :as str]
              [clojure.set :as set]
              [starter.boards :as boards]))

(defn generate-board 
  [inp]
  (let [parts (str/split inp #"\n")
        sub-part (mapv #(vec (mapv vec (re-seq #".{1,3}" %))) parts)]
    sub-part))

(defn filter-non-zero
  [lst]
  ;;takes a vector of vector and return a sequence with no zero occuring in data
  (remove zero? (mapv #(apply int %) (vec (flatten lst)))))

(defn check-board-row
  [board]
  (let [test-vec (filter-non-zero (first board))
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
        chk1 (if (empty? (filter-non-zero sec1))
               true
               (apply distinct? (filter-non-zero sec1)))
        sec2 [(get-in board [0 1]) (get-in board [1 1]) (get-in board [2 1])]
        chk2 (if (empty? (filter-non-zero sec2))
               true
               (apply distinct? (filter-non-zero sec2)))
        sec3 [(get-in board [0 2]) (get-in board [1 2]) (get-in board [2 2])]
        chk3 (if (empty? (filter-non-zero sec3))
               true
               (apply distinct? (filter-non-zero sec3)))] 
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


(defn insert-in
  [board position value]
  ;;returns nil if inserted wrong number at wrong position
  ;; position is vector of 3 elements
  (let [updated-board (assoc-in board position value)
        valid-suduko? (validate-board updated-board)]
    (when valid-suduko?
      updated-board
      )))


(defn original-maintained?
  
  ;;checks the difference between original and new board in place of non zero terms
  [board nboard]
  (let [flatten-org (mapv #(vec (flatten %)) board)
        flatten-new (mapv #(vec (flatten %)) nboard)
        *chk (atom true)]
    
    (loop [i 0
           j 0]
      (when (not= (get-in flatten-org [i j]) "0")
        (when (not= (get-in flatten-org [i j]) (get-in flatten-new [i j]))
          ;;(println i "__" j "::" (get-in flatten-org [i j]) "__" (get-in flatten-new [i j]))
          (reset! *chk false)))
      (when (< i 9)
        (if (= j 8)
          (recur (inc i)
                 0)
          (recur i
                 (inc j)))))
    @*chk))


#_(defn insert-in2
  [board position value]
  (let [to-update (get-in board position)]))


(defn insert-update-board
  ;;returns new board if value can be inserted or returns nil
  [org-board intr-board position value]
  (let [upd-brd (insert-in intr-board position value)
        chk (original-maintained? org-board upd-brd)]
    (if chk
      upd-brd
      false)))

(defn valid-nums-in-sector
  [board position]
  (if-not (< (first position) 3)
    (valid-nums-in-sector (vec (drop 3 board)) (assoc position 0 (- (first position) 3)))
    (let [sec (apply conj [] (map #(get-in board [% (second position)]) (range 3)))]
      (filter-non-zero sec)
      )))


(defn get-valid-nums
  [board position]
  (let [horizontal-nums (filter-non-zero (get board (first position)))
        vertical-nums (filter-non-zero (apply conj [] (mapv #(get-in board [% (second position) (last position)]) (range 9))))
        sector-nums (valid-nums-in-sector board position)
        non-valid-nums (set (concat horizontal-nums vertical-nums sector-nums))]
    (set/difference #{1 2 3 4 5 6 7 8 9} non-valid-nums)
    ))

(defn valid-updation-list
  [org-board board position]
  (when (insert-update-board org-board board position "0")
    (let [flatten-lst (mapv #(vec (flatten %)) board)
          trp-lst (vec (apply map vector flatten-lst))])))


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
        board2 (generate-board "020030090\n000907000\n900208005\n004806500\n607000208\n003102900\n800605007\n000309000\n030020050\n")
        upd-board (insert-in board1 [0 0 1] "0")
        org-true? (original-maintained? board1 upd-board)
        sec-lst (valid-nums-in-sector board1 [7 2 1])
        lst (get-valid-nums board1 [4 0 2])
        ]
    lst)
  (boards/random-board)
  )