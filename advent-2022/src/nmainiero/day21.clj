(ns nmainiero.day21
    (:require
     [clojure.string :as str]
     [ubergraph.core :as uber]))


(def sample "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(def input (slurp "resources/day21.txt"))

(defn to-map [[_ key left operation right value]]
  {:key key :left left :operation operation :right right :value value})

(defn evaluate [input current]
  (if (not (get current :value))
    (let [left (:left current)
          right (:right current)
          op (:operation current)]
         (cond (= op "+")
               (+ (evaluate input (get input left)) (evaluate input (get input right)))
               (= op "-")
               (- (evaluate input (get input left)) (evaluate input (get input right)))
               (= op "*")
               (* (evaluate input (get input left)) (evaluate input (get input right)))
               (= op "/")
               (/ (evaluate input (get input left)) (evaluate input (get input right)))))
    (parse-long (:value current))))
  
 
(defn part-1 [input]
    (let [parsed-input (->> input
                           (str/split-lines)
                           (map #(re-matches #"(\w+):\s(?:(\w+)\s([+-\/*])\s(\w+)|(\d+))" %))
                           (map to-map)
                           (into {} (map (juxt :key identity))))]
      (evaluate parsed-input (get parsed-input "root")))) 

(defn part-2 [input start]
    (let [parsed-input (->> input
                           (str/split-lines)
                           (map #(re-matches #"(\w+):\s(?:(\w+)\s([+-\/*])\s(\w+)|(\d+))" %))
                           (map to-map)
                           (into {} (map (juxt :key identity))))
          end (+ start 150)]
        (doseq [x (range start end 15)]
               (let [modified-input (assoc-in parsed-input ["humn" :value] (str x))]
                 (println x (- (evaluate modified-input (get modified-input (:left (get modified-input "root"))))
                             82091308111060))))))


(comment
 (part-1 sample)
 (part-1 input)
 (part-2 sample 300)
 ; Binary search by hand
 (part-2 input (/ (+ 3412650897415 3412650897185) 2)))

 

 
 
 

  
  
  
  
  
  
  
