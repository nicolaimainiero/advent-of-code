(ns advent-of-code.day-6
  (:require [clojure.string :as str]
            [ubergraph.alg :as uga]
            [ubergraph.core :as ug]))

(defn parse-2 [s]
  (into []
        (map (fn [l]
               (vec (str/split l #"\)"))))
        (str/split-lines s)))


(defn parse [input]
  (map seq (map #(str/split % #"\)") (str/split input #"\n"))))

(def sample "K)L\nCOM)BBB\nBBB)C\nC)D\nD)EEE\nEEE)12F\nBBB)G\nG)H\nD)I\nEEE)J\nJ)K")

(def sample-with-santa "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN")


(def input (slurp "./resources/day6.txt"))
;; => #'advent-of-code.day-6/input

(parse sample)
;; => (("K" "L") ("COM" "BBB") ("BBB" "C") ("C" "D") ("D" "EEE") ("EEE" "12F") ("BBB" "G") ("G" "H") ("D" "I") ("EEE" "J") ("J" "K"))

(defn find-com [orbits]
  (first (filter #(= "COM" (first %)) orbits)))

(find-com (parse sample))


(defn add-children [nodes node]
  (let [children (filter #(= (second node) (first %)) nodes)]
    (if (empty? children)
                                        ; Base case
       (cons (first node) (list (list (second node))))
                                        ; Recursive case
      (cons
       (first node)
       (map #(add-children nodes %) children)))))


(defn build-orbit-graph [input]
  (add-children input (find-com input)))

(build-orbit-graph (parse sample))
;; => ("COM" ("BBB" ("C" ("D" ("EEE" ("12F")) ("EEE" ("J" ("K" ("L"))))) ("D" ("I")))) ("BBB" ("G" ("H"))))

(build-orbit-graph (parse sample-with-santa))
;; => ("COM" ("B" ("C" ("D" ("E" ("F")) ("E" ("J" ("K" ("L")) ("K" ("YOU"))))) ("D" ("I" ("SAN"))))) ("B" ("G" ("H"))))

(defn get-number-of-orbits [orbit-graph]
  (let [walk
        (fn walk [depth node]
          (lazy-seq
           (cons [(first node) depth]
                 (when (seq? node)
                   (mapcat (partial walk (inc depth)) (rest node))))))]
  (walk 0 orbit-graph)))

(get-number-of-orbits (build-orbit-graph (parse sample)))

(defn distinct-by [f coll]
  (let [groups (group-by f coll)]
    (map #(first (groups %)) (distinct (map f coll)))))

(distinct-by first (get-number-of-orbits (build-orbit-graph (parse sample))))


(defn orbit-count-checksum [input]
  (reduce + 0 (map #(second %) (distinct-by first (get-number-of-orbits (build-orbit-graph (parse input)))))))


(orbit-count-checksum sample)
(orbit-count-checksum (slurp "./resources/day6.txt"))
;; => 234446


(defn parse-2 [s]
  (into []
        (map (fn [l]
               (vec (str/split l #"\)"))))
        (str/split-lines s)))

(defn graph [in]
  (apply ug/graph
         in))


(defn part-2 [s]
  (let [g (graph (parse-2 s))]
    (- (uga/cost-of-path (uga/shortest-path g "YOU" "SAN"))
       2)))


(part-2 sample-with-santa)
(part-2 (slurp "./resources/day6.txt"))
;; => 385
