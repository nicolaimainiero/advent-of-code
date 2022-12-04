(ns advent-2020.day4
  (:require [clojure.string :as str]))

(def input
  (str/split-lines
    (slurp "resources/day5.txt")))


(def sample "BFFFBBFRRR")

(take 7 sample)
(drop 7 sample)


(defn back-front-to-binary [char]
  (cond
    (#{\B} char) 1
    (#{\F} char) 0))

(defn left-right-to-binary [char]
  (cond
    (#{\R} char) 1
    (#{\L} char) 0))


(defn seat-string-to-number [seat-string to-binary-mapping]
  (Integer/parseInt
   (apply str (map to-binary-mapping seat-string)) 2))



(defn determine-seat-id [seat-string]
  (let [row-string (take 7 seat-string)
        column-string (drop 7 seat-string)]
    (+ (* 8 (seat-string-to-number row-string back-front-to-binary))
     (seat-string-to-number column-string left-right-to-binary))))

                                        ;BFFFBBF RRR
                                        ; 1000110

(defn highest-seat-id [input]
  (apply max
    (map determine-seat-id input)))

(highest-seat-id input)

(defn sorted-seat-ids [input]
  (sort
   (map determine-seat-id input)))

(sorted-seat-ids input)
