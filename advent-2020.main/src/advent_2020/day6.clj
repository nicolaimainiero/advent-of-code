(ns advent-2020.day6
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn split [regex s]
  (str/split s regex))

(def input
  (slurp "resources/day6.txt"))

(def part-1
  (->> input
     (split #"\n\n")
     (map #(str/replace % "\n" ""))
     (map distinct)
     (map count)
     (reduce +)))
;; => 6430

(->> input
   (split #"\n\n")
   (map #(split #"\n" %))
   (map #(map set %))
   (map #(apply set/intersection %))
   (map count)
   (reduce +))
;; => 3125

;; => 1589
