(ns nmainiero.day4
  (:require [clojure.string :as str]))

(def input (slurp "resources/day4.txt"))

(defn section-contained? [[[ls le] [rs re]]]
  (if
   (or (and (>= ls rs) (<= le re)) (and (>= rs ls) (<= re le))) 1 0))


;; 2 - 4  6 - 8
;; ls le  rs  re
;; rs <= ls <= re or rs <= le <= re
;; ls <= rs <= le or ls <= re <= le

(defn section-overlaps? [[[ls le] [rs re]]]
  (cond
      (<= rs ls re) 1
      (<= rs le re) 1
      (<= ls rs le) 1
      (<= ls re le) 1
      :else 0))

(defn parse-section [section]
  (let [[start-str end-str] (str/split section #"-")
        start (parse-long start-str)
        end (parse-long end-str)]
    [start end]))

(defn already-covered? [cover-fn section-definition]
  (cover-fn (map
             parse-section
             (str/split section-definition #","))))

(defn part-1 [input]
  (->> input
       (str/split-lines)
       (map (partial already-covered? section-contained?))
       (reduce +)))

(defn part-2 [input]
  (->> input
       (str/split-lines)
       (map (partial already-covered? section-overlaps?))
       (reduce +)))

(comment

  (def sample "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

  (already-covered? section-overlaps? "2-4,6-8")

  (part-1 sample)
  (part-1 input)
  (part-2 sample)
  (part-2 input))
