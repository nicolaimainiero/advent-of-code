(ns nmainiero.day3
  (:require
   [clojure.set :as set]
   [clojure.string :as str]))

(def input (slurp "resources/day3.txt"))

(defn priority [char]
  (str/index-of "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" char))

(defn appears-in-both [backpack]
  (let [[left right] (split-at (/ (count backpack) 2) backpack)]
    (first (set/intersection (set left) (set right)))))

(defn part-1 [input]
  (->> input
       (str/split-lines)
       (map appears-in-both)
       (map priority)
       (reduce +)))

(defn appears-in-three [input]
  (let [[first second third] input]
    (set/intersection (set (seq first)) (set (seq second)) (set (seq third)))))

(defn part-2 [input]
  (->> input
       (str/split-lines)
       (partition 3)
       (map appears-in-three)
       (map first)
       (map priority)
       (reduce +)))

(part-1 input)
(part-2 input)


(comment
  (def sample "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

  (part-1 sample)
  (part-1 input)

  (part-2 sample)
  (part-2 input))

