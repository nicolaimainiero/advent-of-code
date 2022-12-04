(ns advent-2020.day4
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.spec.alpha :as s]))

(def valid-passport '{:hgt "181cm", :pid "591597745", :byr "1920", :eyr "2029", :iyr "2010", :ecl "gry", :cid "123", :hcl "#6b5442"})
(def invalid-passport '{:hgt "181cm", :pid "591597745", :eyr "2029", :iyr "2010", :ecl "gry", :cid "123", :hcl "#6b5442"})

(s/def ::passport
  (s/keys :req-un [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
          :opt [::cid]))

(def input
  (str/split (slurp "resources/day4.txt") #"\n\n"))

(defn make-passport [entry]
  (walk/keywordize-keys
   (apply hash-map
          (str/split
           (str/replace entry #"\n" " ") #":|\s"))))

(defn make-passports [entries]
  (map make-passport entries))

(defn valid-passport-spec? [passport]
  (s/valid? ::passport passport))

(defn valid-passport? [{:keys [byr iyr eyr hgt hcl ecl pid]}]
  (and byr iyr eyr hgt hcl ecl pid
    ; byr (Birth Year) - four digits; at least 1920 and at most 2002.
       (<= 1920 (Integer/parseInt byr) 2002)
    ; iyr (Issue Year) - four digits; at least 2010 and at most 2020.
       (<= 2010 (Integer/parseInt iyr) 2020)
    ; eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
       (<= 2020 (Integer/parseInt eyr) 2030)
    ; hgt (Height) - a number followed by either cm or in:
    ;   If cm, the number must be at least 150 and at most 193.
    ;   If in, the number must be at least 59 and at most 76
       (let [[_ number unit] (re-matches #"(\d+)(cm|in)" hgt)]
         (case unit
           "cm" (<= 150 (Integer/parseInt number) 193)
           "in" (<=  59 (Integer/parseInt number) 76)
           false))
    ; hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
       (re-matches #"#[0-9a-f]{6}" hcl)
    ; ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
       (re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" ecl)
    ; pid (Passport ID) - a nine-digit number, including leading zeroes.
       (re-matches #"[0-9]{9}" pid)))
  ; cid (Country ID) - ignored, missing or not.


(count
 (filter valid-passport-spec?
         (make-passports input)))
;; => 254

(count
 (filter valid-passport?
         (make-passports input)))
;; => 184
