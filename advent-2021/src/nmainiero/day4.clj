(ns nmainiero.advent-2021.day4
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]
            [clojure.set :as set]))


(def input
  (slurp "resources/day4.txt"))

(def sample
  (slurp "resources/day4-sample.txt"))

(defn- parse-board [board]
  (mapv #(mapv parse-long (str/split (str/trim % ) #"\s+")) (str/split (str/trim board) #"\n")))


(defn- parse [in]
  (let [[first rest] (str/split in #"\n" 2)]
    {:numbers (map parse-long (str/split first #","))
     :boards (mapv parse-board (str/split rest #"\n\n"))}))

(defn- contains-all? [collection keys]
  (.containsAll collection keys))


(defn- did-board-win? [numbers board]
  (let [rows board
        columns (matrix/transpose board)]
    (when (or (some #(contains-all? numbers %) rows)
             (some #(contains-all? numbers %) columns)) board)))

(defn- find-winning-board [input]
  (let [numbers (:numbers (parse input))
        boards (:boards (parse input))]
    (loop [available-numbers numbers
           selected-numbers []
           boards boards]
      (if (some #(did-board-win? selected-numbers %) boards) {:selected-numbers selected-numbers :winning-board (some #(did-board-win? selected-numbers %) boards)}
          (recur (rest available-numbers) (conj selected-numbers (first available-numbers)) boards)))))

(defn- find-loosing-board [input]
  (let [numbers (:numbers (parse input))
        boards (:boards (parse input))]
    (loop [available-numbers numbers
           selected-numbers []
           boards boards]
      (if (= 1 (count boards)) {:selected-numbers selected-numbers :loosing-board (first boards)}
          (recur (rest available-numbers) (conj selected-numbers (first available-numbers)) (remove #(did-board-win? selected-numbers %) boards))))))


(defn step-1 [input]
  (let [solution (find-winning-board input)
        numbers (:selected-numbers solution)
        significant-number (last numbers)
        numbers-winning-board (flatten (:winning-board solution))]
    (* significant-number
       (reduce + (set/difference (into #{} numbers-winning-board) numbers)))))

(defn step-2 [input]
  (let [solution (find-loosing-board input)
        numbers (:selected-numbers solution)
        significant-number (last numbers)
        numbers-winning-board (flatten (:loosing-board solution))]
    (println significant-number)
    (println numbers-winning-board)
    (println (set/difference (into #{} numbers-winning-board) numbers))
    (* significant-number
       (reduce + (set/difference (into #{} numbers-winning-board) numbers)))))


(comment

  (step-1 input)

  (step-2 input)

  (let [solution (find-loosing-board sample)
        numbers (:selected-numbers solution)
        significant-number (last numbers)
        numbers-winning-board (flatten (:loosing-board solution))]
    (* significant-number
       (reduce + (set/difference (into #{} numbers-winning-board) numbers))))

  (flatten (:winning-board (find-winning-board sample)))

  (def board "14 21 17 24  4\n10 16 15  9 19\n18  8 23 26 20\n22 11 13  6  5\n 2  0 12  3  7")
  (.containsAll [7,4,9,5,11,17,23,2,0,14,21] [14 21 17 24  4])

  (def row " 8  2 23  4 24")
  (str/split (str/trim row) #"\s+")

  (:boards (parse sample))
  (some #(contains-all? [7,4,9,5,11,17,23,2,0,14,21, 24] %) (parse-board board))

  (did-board-win? [7,4,9,5,11,17,23,2,0,14,21] (parse-board board)))
;; => nil
;; => nil
;; => nil
  

