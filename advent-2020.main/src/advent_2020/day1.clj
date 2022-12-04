(ns advent-2020.day1
  (:require [clojure.string :as str]))

(def input
  (map #(Integer/parseInt %)
       (str/split-lines
          (slurp "resources/day1.txt"))))


(for [x input y input :let [sum (+ x y)] :when (= sum 2020)] [sum x y (* x y)])


(for [x input y input z input :let [sum (+ x y z)] :when (= sum 2020)] [sum x y z (* x y z)])


