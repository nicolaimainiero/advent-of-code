(ns advent-2020.day8
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (slurp "resources/day8.txt"))


(defn parse-instruction [serialized]
  (let [[_ instruction arg]
        (re-matches #"(jmp|acc|nop) ([+-]\d+)" serialized)]
    [(keyword instruction) (Integer/parseInt arg)]))


(defn execute [program]
  (loop [acc 0 pc 0 visited #{}]
    (if (contains? visited pc)
      {:status :loop :accumulator acc}
      (if (= pc (count program))
        {:status :halts :accumulator acc}
        (let [[operation argument] (program pc)]
          (case operation
            :acc (recur (+ acc argument) (inc pc) (conj visited pc))
            :jmp (recur acc (+ pc argument) (conj visited pc))
            :nop (recur acc (inc pc) (conj visited pc))))))))

; part 1
(->> input
     (str/split-lines)
     (mapv parse-instruction)
     execute)
;; => 1766


(let [program (->> input
                   str/split-lines
                   (mapv parse-instruction))]
  (->> program
     count
     range
     (map #(update-in program [% 0] {:jmp :nop :nop :jmp :acc :acc}))
     (map execute)
     (filter #(= :halts (:status %)))))
