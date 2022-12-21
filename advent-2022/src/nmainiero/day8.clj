(ns nmainiero.day8
  (:require [clojure.string :as str]))

(def sample "30373
25512
65332
33549
35390")

(def input (slurp "resources/day8.txt"))


(defn take-while-and-one
  [pred coll]
  (lazy-seq
   (when-let [s (seq coll)]
     (if (pred (first s))
       (cons (first s) (take-while-and-one pred (rest s)))
       (list (first s))))))

(defn is-visible? [[left tree right]]
  (let [test #(> tree %)]
    (or (every? test left) (every? test right))))

(defn scenic-score [[left tree right]]
  (let [test #(> tree %)
        score #(count (take-while-and-one test %))]
    ;(println [(reverse left) (score (reverse left)) tree right (score right)])
    (* (score (reverse left)) (score right))))

(defn partition-trees [row idx] [(take idx row) (nth row idx) (drop (inc idx) row)])

(defn row-and-column [input idx-row idx-column]
  (let [row (nth input idx-row)
        column (map #(nth % idx-column) input)]
    [(partition-trees row idx-column) (partition-trees column idx-row)]))


(defn parse [input]
  (map (fn [row] (map parse-long (str/split row #"")))
       (str/split-lines input)))

(defn part-1 [input]
  (let [forrest (parse input)]
    (count (filter true? (for [x (range 0 (count forrest))
                               y (range 0 (count (first forrest)))]
                           (let [[row col] (row-and-column forrest x y)]
                             (some true? [(is-visible? row) (is-visible? col)])))))))


(defn part-2 [input]
  (let [forrest (parse input)]
    (apply max (for [x (range 1 (dec (count forrest)))
          y (range 1 (dec (count (first forrest))))]
       (let [[row col] (row-and-column forrest x y)]
        (* (scenic-score row) (scenic-score col)))))))

(comment

  (part-1 sample)
  (part-1 input)

  (part-2 sample)
  (part-2 input)

)
