(ns advent-2020.day7
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def input
  (slurp "resources/day7.txt"))


                                        ; input dotted blue bags contain 3 wavy bronze bags, 5 clear tomato bags.
(defn parse-bag [line]
  (let [[[_ _ container] & elements]
        (re-seq #"(?:^|(\d+) )(\w+ \w+) bags?" line)]
    {container
     (map (fn [[_ number color]] [(Integer/parseInt number) color]) elements)}))

(parse-bag "dotted blue bags contain 3 wavy bronze bags, 5 clear tomato bags")


(def database
  (->> (slurp "resources/day7.txt")
     (str/split-lines)
     (map parse-bag)
     (apply merge)))


(defn bag-contains? [container own-color]
  (->> container
       database
       (some (fn [[_ color]] (or
                              (= color own-color)
                              (bag-contains? color own-color))))))

(->> database
     (filter (fn [[container elements]]
               (bag-contains? container "shiny gold")))
     count)

(defn bag-count [container]
  (->> container
       database
       (reduce (fn [acc [n color]]
                 (+ acc n
                    (* n (bag-count color))))
        0)))

(bag-count "shiny gold")
