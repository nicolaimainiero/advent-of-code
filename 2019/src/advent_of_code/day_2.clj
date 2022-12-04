(ns advent-of-code.day-2
   (:require [clojure.string :as str]))


(defn parse [input]
  (vec (map  #(Integer/parseInt %)
                       (map str/trim (str/split input #",")))))

(def input
  (parse (slurp "resources/day2.txt")))

(defn get-current-instruction [instruction-pointer state]
  (take 4 (drop instruction-pointer state)))

(defn process-instruction [instruction-pointer memory]
  ; part one
  (let [[opcode op-one op-two target] (get-current-instruction instruction-pointer memory)]
    (cond (= opcode 1)
          (let [new-memory (vec (assoc memory target (+ (nth memory op-one) (nth memory op-two))))]
          ; let next-state left hand side and get wirh get-current-instruction next instruction
               (process-instruction (+ instruction-pointer 4) new-memory))
          (= opcode 2)
          (let [new-memory  (vec (assoc memory target (* (nth memory op-one) (nth memory op-two))))]
               (process-instruction (+ instruction-pointer 4) new-memory))
        (= opcode 99) memory
        :else "error")))

(def sample (parse "1,9,10,3,2,3,11,0,99,30,40,50"))
(def sample-1 (parse "1,0,0,0,99"))
(def sample-2 (parse "2,3,0,3,99"))

(process-instruction 0 sample)
(process-instruction 0 sample-1)
(process-instruction 0 sample-2)
(first (process-instruction 0 (assoc input 1 12 2 2)))

(defn part-two []
  (for [noun (range 0 100)
      verb (range 0 100)
      :let [initial (+ (* 100 noun) verb)]
      :when (= (first (process-instruction 0 (assoc input 1 noun 2 verb))) 19690720)]
  initial))
