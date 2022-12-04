(ns advent-2020.day3
  (:require [clojure.string :as str]))

(def input
  (str/split-lines (slurp "resources/day3.txt")))

(defn hill [right]
  (let [width (count (first input))
        columns (iterate #(mod (+ right %) width) 0)
        spots (map nth input columns)]
    (count (filter #(= \# %) spots))))

(defn steep-hill [right]
  (let [width (count (first input))
        columns (iterate #(mod (+ right %) width) 0)
        steep-input (take-nth 2 input)
        spots (map nth steep-input columns)]
    (count (filter #(= \# %) spots))))

(*
 (hill 3)
 (hill 1)
 (hill 5)
 (hill 7)
 (steep-hill 1))
;; => 3847183340


