(ns nmainiero.advent-2021
  (:gen-class)
  (:require [clojure.string :as str]))

(def sample
  [199 200 208 210 200 207 240 269 260 263])

(def input
  (map #(Integer/parseInt %)
       (str/split-lines
        (slurp "resources/day1.txt"))))

(defn step-1 [in]
  ;; Get the amount of depth increases
  (get (frequencies
        (map #(< (first %) (second %))
          (partition 2 1 (into [Integer/MAX_VALUE] in)))) true))

(step-1 input)

(defn step-2 [in]
  ;; like step-1 with sliding window over three consecutive values
  (step-1
   (map #(reduce + %)
    (partition 3 1 in))))

(step-2 input)

(comment
  ;; Check if depth increased for given pair
  (map #(< (first %) (second %))
    (partition 2 1 (into [Integer/MAX_VALUE] sample)))
   ;; => (false true true true false true true true false true)

  ;; Create consecutive pairs of depth, start with Integer/MAX_VALUE because first reading never increases depth
  (partition 2 1 (into [Integer/MAX_VALUE] sample))
  ;; => ((2147483647 199) (199 200) (200 208) (208 210) (210 200) (200 207) (207 240) (240 269) (269 260) (260 263))

  ;; sliding window
  (map #(reduce + %)
   (partition 3 1 sample)))
