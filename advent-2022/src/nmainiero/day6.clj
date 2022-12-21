(ns nmainiero.day6)

(def sample "mjqjpqmgbljsphdztnvjfqwrcgsmlb")

(def input (slurp "resources/day6.txt"))

(defn start-of-packet-marker? [[idx packet]]
  [idx (= 4 (count (set packet)))])

(defn start-of-message-marker? [[idx packet]]
  [idx (= 14 (count (set packet)))])

(defn part-1 [datastream]
  (+ 4 (ffirst
        (drop-while (fn [[_ value]] (not value)) (map start-of-packet-marker? (map-indexed vector (partition 4 1 datastream)))))))


(defn part-2 [datastream]
  (+ 14 (ffirst (drop-while (fn [[_ value]] (not value)) (map start-of-message-marker? (map-indexed vector (partition 14 1 datastream)))))))


(comment
  (part-1 sample)
  (part-1 input)
  (part-2 sample)
  (part-2 input)
)
