(ns starter.struct
  (:require   [clojure.string :as str]
              [clojure.set :as set]
              #_[starter.boards :as boards]))

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
  (random-board)
  )
























(def grids ["003020600\n900305001\n001806400\n008102900\n700000008\n006708200\n002609500\n800203009\n005010300\n"
            "200080300\n060070084\n030500209\n000105408\n000000000\n402706000\n301007040\n720040060\n004010003\n"
            "000000907\n000420180\n000705026\n100904000\n050000040\n000507009\n920108000\n034059000\n507000000\n"
            "030050040\n008010500\n460000012\n070502080\n000603000\n040109030\n250000098\n001020600\n080060020\n"
            "020810740\n700003100\n090002805\n009040087\n400208003\n160030200\n302700060\n005600008\n076051090\n"
            "100920000\n524010000\n000000070\n050008102\n000000000\n402700090\n060000000\n000030945\n000071006\n"
            "043080250\n600000000\n000001094\n900004070\n000608000\n010200003\n820500000\n000000005\n034090710\n"
            "480006902\n002008001\n900370060\n840010200\n003704100\n001060049\n020085007\n700900600\n609200018\n"
            "000900002\n050123400\n030000160\n908000000\n070000090\n000000205\n091000050\n007439020\n400007000\n"
            "001900003\n900700160\n030005007\n050000009\n004302600\n200000070\n600100030\n042007006\n500006800\n"
            "000125400\n008400000\n420800000\n030000095\n060902010\n510000060\n000003049\n000007200\n001298000\n"
            "062340750\n100005600\n570000040\n000094800\n400000006\n005830000\n030000091\n006400007\n059083260\n"
            "300000000\n005009000\n200504000\n020000700\n160000058\n704310600\n000890100\n000067080\n000005437\n"
            "630000000\n000500008\n005674000\n000020000\n003401020\n000000345\n000007004\n080300902\n947100080\n"
            "000020040\n008035000\n000070602\n031046970\n200000000\n000501203\n049000730\n000000010\n800004000\n"
            "361025900\n080960010\n400000057\n008000471\n000603000\n259000800\n740000005\n020018060\n005470329\n"
            "050807020\n600010090\n702540006\n070020301\n504000908\n103080070\n900076205\n060090003\n080103040\n"
            "080005000\n000003457\n000070809\n060400903\n007010500\n408007020\n901020000\n842300000\n000100080\n"
            "003502900\n000040000\n106000305\n900251008\n070408030\n800763001\n308000104\n000020000\n005104800\n"
            "000000000\n009805100\n051907420\n290401065\n000000000\n140508093\n026709580\n005103600\n000000000\n"
            "020030090\n000907000\n900208005\n004806500\n607000208\n003102900\n800605007\n000309000\n030020050\n"
            "005000006\n070009020\n000500107\n804150000\n000803000\n000092805\n907006000\n030400010\n200000600\n"
            "040000050\n001943600\n009000300\n600050002\n103000506\n800020007\n005000200\n002436700\n030000040\n"
            "004000000\n000030002\n390700080\n400009001\n209801307\n600200008\n010008053\n900040000\n000000800\n"
            "360020089\n000361000\n000000000\n803000602\n400603007\n607000108\n000000000\n000418000\n970030014\n"
            "500400060\n009000800\n640020000\n000001008\n208000501\n700500000\n000090084\n003000600\n060003002\n"
            "007256400\n400000005\n010030060\n000508000\n008060200\n000107000\n030070090\n200000004\n006312700\n"
            "000000000\n079050180\n800000007\n007306800\n450708096\n003502700\n700000005\n016030420\n000000000\n"
            "030000080\n009000500\n007509200\n700105008\n020090030\n900402001\n004207100\n002000800\n070000090\n"
            "200170603\n050000100\n000006079\n000040700\n000801000\n009050000\n310400000\n005000060\n906037002\n"
            "000000080\n800701040\n040020030\n374000900\n000030000\n005000321\n010060050\n050802006\n080000000\n"
            "000000085\n000210009\n960080100\n500800016\n000000000\n890006007\n009070052\n300054000\n480000000\n"
            "608070502\n050608070\n002000300\n500090006\n040302050\n800050003\n005000200\n010704090\n409060701\n"
            "050010040\n107000602\n000905000\n208030501\n040070020\n901080406\n000401000\n304000709\n020060010\n"
            "053000790\n009753400\n100000002\n090080010\n000907000\n080030070\n500000003\n007641200\n061000940\n"
            "006080300\n049070250\n000405000\n600317004\n007000800\n100826009\n000702000\n075040190\n003090600\n"
            "005080700\n700204005\n320000084\n060105040\n008000500\n070803010\n450000091\n600508007\n003010600\n"
            "000900800\n128006400\n070800060\n800430007\n500000009\n600079008\n090004010\n003600284\n001007000\n"
            "000080000\n270000054\n095000810\n009806400\n020403060\n006905100\n017000620\n460000038\n000090000\n"
            "000602000\n400050001\n085010620\n038206710\n000000000\n019407350\n026040530\n900020007\n000809000\n"
            "000900002\n050123400\n030000160\n908000000\n070000090\n000000205\n091000050\n007439020\n400007000\n"
            "380000000\n000400785\n009020300\n060090000\n800302009\n000040070\n001070500\n495006000\n000000092\n"
            "000158000\n002060800\n030000040\n027030510\n000000000\n046080790\n050000080\n004070100\n000325000\n"
            "010500200\n900001000\n002008030\n500030007\n008000500\n600080004\n040100700\n000700006\n003004050\n"
            "080000040\n000469000\n400000007\n005904600\n070608030\n008502100\n900000005\n000781000\n060000010\n"
            "904200007\n010000000\n000706500\n000800090\n020904060\n040002000\n001607000\n000000030\n300005702\n"
            "000700800\n006000031\n040002000\n024070000\n010030080\n000060290\n000800070\n860000500\n002006000\n"
            "001007090\n590080001\n030000080\n000005800\n050060020\n004100000\n080000030\n100020079\n020700400\n"
            "000003017\n015009008\n060000000\n100007000\n009000200\n000500004\n000000020\n500600340\n340200000\n"
            "300200000\n000107000\n706030500\n070009080\n900020004\n010800050\n009040301\n000702000\n000008006"])

(defn random-board
  []
  (generate-board (rand-nth grids)))