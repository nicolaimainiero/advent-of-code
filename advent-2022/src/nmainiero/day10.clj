(ns nmainiero.day10
  (:require [clojure.string :as str]))

(def sample "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop")

(def input (slurp "resources/day10.txt"))


(def initial-state {:x 1 :cycle 0 :accumulated-readings 0 :remaining-cycles 0 :operand 0})

(defn process-addition [state]
  (cond (= 20 (:cycle state)) (println state)
        (= 60 (:cycle state)) (println state)
        (= 100 (:cycle state)) (println state)
        (= 140 (:cycle state)) (println state)
        (= 180 (:cycle state)) (println state)
        (= 220 (:cycle state)) (println state))
  (if (= 0 (:remaining-cycles state))
    (-> state
        (update :x + (:operand state))
        (assoc :operand 0))
    (recur (-> state
               (update :remaining-cycles dec)
               (update :cycle inc)))))

(defn update-state [state [command operand]]
  #_(cond (= 100 (:cycle state)) (println state))
    (cond (= command "noop")
        (update state :cycle inc)
        (= command "addx")
        (process-addition (-> state
                              (update :cycle inc)
                              (update :remaining-cycles + 1)
                              (assoc :operand operand)))))

(defn evaulate [state command remaining-commands]
  (let [new-state (update-state state command)]
    (if (seq remaining-commands)
      (recur new-state (first remaining-commands) (rest remaining-commands))
      new-state)))

(defn parse-line [line] (let [[_ _ command operand] (re-matches #"((noop|addx)(\s[-]*\d+)*)" line)]
                          [command (if (some? operand) (parse-long (str/trim operand)) "")]))


(defn part-1 [input]
  (let [parsed-input (map parse-line (str/split-lines input))]
    (evaulate initial-state (first parsed-input) (rest parsed-input))))

(comment
  (part-1 sample)
  (part-1 input)

  (+ (* 21 20) (* 21 60) (* 40 100) (* 21 140) (* 21 180) (* 21 220))
)
