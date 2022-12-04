
(ns nmainiero.advent-2021.day5
  (:require [clojure.string :as str]
            [clojure.core.matrix :as matrix]
            [clojure.set :as set]))


(defrecord Vent [x1 y1 x2 y2])

(def sample (slurp "resources/day5-sample.txt"))

(defn parse-vent [s]
  (let [[x1 y1 x2 y2] (->>
                       (re-matches #"(\d),(\d)\s*->\s*(\d),(\d)" s)
                       (next)
                       (mapv parse-long))]
    (Vent. x1 y1 x2 y2)))

(defn parse-input [input]
  (-> input
      (str/trim)
      (str/split #"\n")
      (->> (mapv parse-vent))))

(comment

  (def m [[1 0 0] [1 0 0]])

  (def coordinate-pair [[0 9] [5 9]])

  (parse-input sample)

  (def vents (filter #(or (= (:x1 %) (:x2 %)) (= (:y1 %) (:y2 %))) (parse-input sample)))

  (let [width ()])

  (matrix/add m m))
;; => nil
