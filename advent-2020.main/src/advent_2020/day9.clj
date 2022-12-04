(ns advent-2020.day9
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (slurp "resources/day9.txt"))


(def data
  (->> input
       str/split-lines
       (map #(biginteger %))))

(defn split-numbers [sequence]
  (let [numbers (take 25 sequence) sum (last sequence)]
    [numbers sum]))

(defn two-sum [[nums target]]
  (let [nums-index (zipmap nums (range))
        indexs (for [[x i] nums-index
                     [y j] nums-index
                     :when (< i j)
                     :when (= (+ x y) target)]
                target)]
    (first indexs)))


(def code
  (->> data
     (partition 26 1)
     (map split-numbers)
     (map two-sum)))

(first
 (drop-while #(= (first %) (second %))
             (zipmap (drop 25 data) code)))
;; => [731031916 nil]
