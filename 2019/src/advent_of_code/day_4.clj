(ns advent-of-code.day-4)


(def digits-ascending?
  (fn [d]
    (loop [acc true digits d]
      (if (= 1 (count digits))
        acc
        (recur (and acc (<= (first digits) (second digits))) (rest digits)) 
        ))))


(def adj-digits-same?
  (fn [d]
    (loop [acc false digits d]
      (if (= 1 (count digits))
        acc
        (recur (or acc (= (first digits) (second digits))) (rest digits)) 
        ))))
 

(defn digits [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))


(let [digits '(1 2 2 2 4)]
  (adj-digits-same? digits))

(let [digits '(1 2 2 2 4)]
  (digits-ascending? digits))



(defn exactly-two-adj? [d]
  (some #(= 2 %) (map count (partition-by identity d))))

(let [digits '(1 2 2 3 4)]
  (exactly-two-adj? digits))


(defn part-one []
  (count (filter adj-digits-same? (filter digits-ascending? (map digits (range 372037 905157))))))

(defn part-two []
  (count (filter exactly-two-adj? (filter digits-ascending? (map digits (range 372037 905157))))))

(part-one);; => 481

(part-two);; => 299
