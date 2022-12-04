(ns nmainiero.day2
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]))


(def input (slurp "resources/day2.txt"))


(defn score-game
  "Scores a game. Rock (A, X, 1), Paper (B, Y, 2), Scissors (C, Z, 3). Lose (0), Draw (3), Won (6)."
  [game]
  (match [game]
    ["A X"] (+ 1 3)
    ["A Y"] (+ 2 6)
    ["A Z"] (+ 3 0)
    ["B X"] (+ 1 0)
    ["B Y"] (+ 2 3)
    ["B Z"] (+ 3 6)
    ["C X"] (+ 1 6)
    ["C Y"] (+ 2 0)
    ["C Z"] (+ 3 3)))


(defn strategy
  "Select play accoring to second column. X -> loose, Y -> draw, Z -> win"
  [game]
  (match [game]
  ["A X"] "A Z"
  ["A Y"] "A X"
  ["A Z"] "A Y"
  ["B X"] "B X"
  ["B Y"] "B Y"
  ["B Z"] "B Z"
  ["C X"] "C Y"
  ["C Y"] "C Z"
  ["C Z"] "C X"))

(defn part-1 [input]
  (->> input
       (str/split-lines)
       (map score-game)
       (reduce +)))

(defn part-2 [input]
  (->> input
       (str/split-lines)
       (map strategy)
       (map score-game)
       (reduce +)))



(comment

  (def sample "A Y\nB X\nC Z")

  (part-1 sample)
  (part-1 input)

  (part-2 sample)
  (part-2 input)
  )
