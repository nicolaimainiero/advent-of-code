(ns nmainiero.advent-2021.day2
   (:require [clojure.string :as str]))

(def sample
  [["forward" 5]
   ["down" 5]
   ["forward" 8]
   ["up" 3]
   ["down" 8]
   ["forward" 2]])

(def input
  (map (fn [x] [(first x) (Integer/parseInt (second x))])
       (map #(str/split % #" ")
         (str/split-lines (slurp "resources/day2.txt")))))



(defn- navigate [[x y] [direction amount]]
  ;;(println  "x: " x "y: " y)
  (cond (= "forward" direction) [(+ x amount) y]
        (= "down" direction) [x (+ y amount)]
        (= "up" direction) [x (- y amount)]
        :else [x y]))

(defn- advanced-navigate [[x y aim] [direction amount]]
  ;;(println  "x: " x "y: " y "aim: " aim)
  (cond (= "forward" direction) [(+ x amount) (+ y (* aim amount)) aim] 
        (= "down" direction) [x  y (+ aim amount)]
        (= "up" direction) [x  y (- aim amount)]
        :else [x y aim]))

(defn step-1 [in]
  (let [position (reduce navigate [0 0] in)]
    (* (first position) (second position))))

(defn step-2 [in]
  (let [position (reduce advanced-navigate [0 0 0] in)]
    (* (first position) (second position))))


(comment

  (step-1 input)
  (step-2 input)

  (let [position (reduce navigate [0 0] sample)]
    (* (first position) (second position)))

  (let [position (reduce advanced-navigate [0 0 0] sample)]
     (* (first position) (second position)))

  (map (fn [x] [(first x) (Integer/parseInt (second x))]) sample)

  (map (fn [x] [(first x) (Integer/parseInt (second x))])
    (map #(str/split % #" ")
      (str/split-lines (slurp "resources/day2.txt")))))
