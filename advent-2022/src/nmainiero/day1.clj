(ns nmainiero.day1
  (:require [clojure.string :as str]))

(def sample "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000")

(def part-1-input (slurp "resources/day1.txt"))


(defn list-with-calories [input]
  (->> input
       str/split-lines
       (map parse-long)
       (partition-by nil?)
       (map #(reduce + %))
       (filter (complement nil?))))

(defn part-1 [input]
  (apply max (list-with-calories input)))

(defn part-2 [input]
  (->> (list-with-calories input)
       (sort)
       (reverse)
       (take 3)
       (reduce +)))

(comment
  (part-1 sample)
  (part-1 part-1-input)
  (part-2 sample)
  (part-2 part-1-input))
