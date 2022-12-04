(ns advent-2020.day2
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (take-last 4
             (re-matches #"(\d+)-(\d+)\s(\w):\s(\w+)" line)))

(def input
  (map parse-line
       (str/split-lines
         (slurp "resources/day2.txt"))))

(defn valid-password-rule [password]
  (let [freq  (frequencies (last password))
        min (Integer/parseInt (first password))
        max (Integer/parseInt (second password))
        letter (first (nth password 2))]
    (and (not (nil? (get freq letter)))
         (>= max (get freq letter))
         (<= min (get freq letter)))))


(defn valid-password-rule-1 [password]
  (let [pwd (last password)
        pos-1 (- (Integer/parseInt (first password)) 1)
        pos-2 (- (Integer/parseInt (second password)) 1)
        letter (first (nth password 2))]
    (and (not (and (= letter (nth pwd pos-1))
                   (= letter (nth pwd pos-2))))
         (or (= letter (nth pwd pos-1))
             (= letter (nth pwd pos-2))))))



(defn day2-one [list]
  (count (filter true? (map valid-password-rule list))))

(defn day2-two [list]
  (count (filter true? (map valid-password-rule-1 list))))
