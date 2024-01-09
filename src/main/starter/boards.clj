(ns starter.boards 
  (:require [clojure.core :as core]
            [clojure.string :as s]
            [starter.struct :as struct]))

(defn extract-grids [file-path]
  (let [file-content (core/slurp file-path)
        f (s/split file-content #"(?m)(?=^Grid \d{2}$)")
        s (apply conj [] (mapv #(apply str (drop 8 %)) f))]
    s))

(def grids (extract-grids "/home/gurpreet/suduko/suduko-player/src/main/starter/grids.txt"))

(defn random-board
  []
  (struct/generate-board (rand-nth grids))
  )

(comment 
  (random-board)
  )
