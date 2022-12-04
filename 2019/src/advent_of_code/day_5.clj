(ns advent-of-code.day-5
    (:require [clojure.string :as str]))

(defn parse [input]
  (vec (map  #(Integer/parseInt %)
             (map str/trim (str/split input #",")))))

(def input
  {:instruction-counter 0
   :memory (parse (slurp "resources/day5.txt"))})

(def sample-state {:instruction-counter 0 :memory [1002 4 3 4 33]})

(def part-2-example-1
  {:instruction-counter 0
   :memory (parse "3,9,8,9,10,9,4,9,99,-1,8")})

(def jump-test-position
  {:instruction-counter 0
   :memory (parse "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9")})

(def jump-test-immediate
  {:instruction-counter 0
   :memory (parse "3,3,1105,-1,9,1101,0,0,12,4,12,99,1")})

(def compare-test-position
  {:instruction-counter 0
   :memory (parse "3,9,8,9,10,9,4,9,99,-1,8")})


(defn- current-instruction
  [state]
  (format "%05d" (get (state :memory) (state :instruction-counter))))

(defn get-current-opcode [state]
  (apply str
         (take-last 2
                    (current-instruction state))))

(defn get-operand-mode [state operand]
  (nth (current-instruction state) (- 3 operand)))

(defn get-operand-value [state operand]
  (let [mode (get-operand-mode state operand) absolute-operand-position (+ (state :instruction-counter) operand)]
    (cond (= mode \0) (nth (state :memory) (nth (state :memory) absolute-operand-position))
          (= mode \1) (nth (state :memory) absolute-operand-position))))

(get-operand-value sample-state 3) ;; => 33

(defn get-write-position [state position]
  (nth (state :memory) (+ (state :instruction-counter) position)))

(get-write-position sample-state 3) ;; => 4

(def process
  (fn [state]
    (cond (= "99" (get-current-opcode state)) :end
          (= "01" (get-current-opcode state))
          (recur (update-in (assoc-in state [:memory (get-write-position state 3)] (+ (get-operand-value state 1) (get-operand-value state 2))) [:instruction-counter] + 4))
          (= "02" (get-current-opcode state))
          (recur (update-in (assoc-in state [:memory (get-write-position state 3)] (* (get-operand-value state 1) (get-operand-value state 2))) [:instruction-counter] + 4))
          (= "03" (get-current-opcode state)) ; input
          (let [input (Integer/parseInt (read-line))] (recur (update-in (assoc-in state [:memory (get-write-position state 1)] input) [:instruction-counter] + 2)))
          (= "04" (get-current-opcode state)) ; output
          (let [output (println (get-operand-value state 1))] (recur (update-in state [:instruction-counter] + 2)))
          (= "05" (get-current-opcode state)) ; jump-if-true
          (recur (if (not= 0 (get-operand-value state 1)) (assoc-in state [:instruction-counter] (get-operand-value state 2)) (update-in state [:instruction-counter] + 3)))
          (= "06" (get-current-opcode state)) ; jump-if-false
          (recur (if (= 0 (get-operand-value state 1)) (assoc-in state [:instruction-counter] (get-operand-value state 2)) (update-in state [:instruction-counter] + 3)))
          (= "07" (get-current-opcode state)) ; less than
          (recur
           (let [less-than (if (< (get-operand-value state 1) (get-operand-value state 2)) 1 0)]
             (update-in (assoc-in state [:memory (get-write-position state 3)] less-than) [:instruction-counter] + 4)))
          (= "08" (get-current-opcode state)) ; equals
          (recur
           (let [equals (if (= (get-operand-value state 1) (get-operand-value state 2)) 1 0)]
                          (update-in (assoc-in state [:memory (get-write-position state 3)] equals) [:instruction-counter] + 4)))
          :else (println "Found unknown operand: " (get-current-opcode state)))))


(defn part-1 []
  (process input))


(comment
 (process jump-test-position)
 (process jump-test-immediate))

(part-1)

