(ns nmainiero.advent-2021.day3
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]))


(def sample
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])

(def input
  (str/split-lines (slurp "resources/day3.txt")))

(defn- most-common [x]
        (if (>= (get x "1") (get x "0")) 1 0))

(defn- least-common [x]
  (if (< (get x "1") (get x "0")) 1 0))


(defn step-1 [in]
  (let [x (map #(frequencies %)
               (matrix/transpose
                (map #(str/split % #"")
                     in)))
        gamma-rate (Integer/parseInt
                    (apply str
                           (map most-common x)) 2)
        epsilon-rate(Integer/parseInt
                     (apply str
                            (map least-common x)) 2)]
    (* gamma-rate epsilon-rate)))


(defn rating [comparator splitted-input c]
  (if (= 1 (count splitted-input))
    (first splitted-input)
    (let [mc (comparator (nth (map #(frequencies %)
                               (matrix/transpose
                                splitted-input)) c))]
      (if (= 1 (count splitted-input))
        (first splitted-input)
        (recur comparator
         (filter #(= mc (Integer/parseInt (nth % c))) splitted-input) (inc c))))))


(defn step-2 [in]
  (let [splitted-input  (map #(str/split % #"") in)
        oxygen-generator-rating (Integer/parseInt
                                 (apply str
                                        (rating most-common splitted-input 0)) 2)
        co2-scrubber-rating (Integer/parseInt
                             (apply str
                                    (rating least-common splitted-input 0)) 2)]
    (* oxygen-generator-rating co2-scrubber-rating)))


(comment

  (step-1 input)


  (step-2 input)

  (count (filter #(= 1 (nth % 4)) [[1 0 1 1 0] [1 0 1 1 1]]))

 (nth (map #(frequencies %)
           (matrix/transpose [[1 0 1 1 0] [1 0 1 1 1]])) 4)

 (get {0 1, 1 1} "1")

 (nth [1 0 1 1 1] 4)


 (filter #(= 0 (Integer/parseInt (first %)))
   (map #(str/split % #"") sample))

 (most-common
   (first (map #(frequencies %)
            (matrix/transpose
             (map #(str/split % #"")
               sample)))))

 (map #(str/split % #"")
      sample)


 (Integer/parseInt
   (apply str
     (map most-common (map #(frequencies %)
                        (matrix/transpose
                             (map #(str/split % #"")
                               sample))))) 2))
;; => nil


