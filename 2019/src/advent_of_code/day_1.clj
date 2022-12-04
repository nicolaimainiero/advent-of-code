(ns advent-of-code.day-1
  (:require [clojure.string :as str]))

(def input
  (map #(Integer/parseInt %)
       (str/split-lines
        (slurp "resources/day1.txt"))))

(defn modulemass-to-fuel [mass]
  (- (Math/floor (/ mass 3)) 2))


(modulemass-to-fuel 1969)
;; => 654.0

(defn advanced-modulemass-to-fuel [mass]
  (let [fuel (modulemass-to-fuel mass)]
  (cond
    (> fuel 0) (+ fuel (advanced-modulemass-to-fuel fuel))
    (<= fuel 0) 0)))

(advanced-modulemass-to-fuel 1969)
;; => 966.0
(advanced-modulemass-to-fuel 100756)
;; => 50346.0

(defn part-one [input]
  (reduce + (map modulemass-to-fuel input)))

(part-one input)

(defn part-two [input]
  (reduce + (map advanced-modulemass-to-fuel input)))

(part-two input)
;; => 5106629.0
