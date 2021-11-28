(ns aoc.year2020.day04
  (:require [aoc.util :as aoc]
            [clojure.string :as str]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]))


(s/def ::byr (s/int-in 1920 2003))
(s/def ::iyr (s/int-in 2010 2021))
(s/def ::eyr (s/int-in 2020 2031))

(s/def ::hgt (s/or :cm (s/cat :unit #{:cm} :value (s/int-in 150 194))
                   :in (s/cat :unit #{:in} :value (s/int-in 59 77))))

(s/def ::hcl (s/tuple (s/int-in 0 256) (s/int-in 0 256) (s/int-in 0 256)))
(s/def ::ecl #{"amb"
               "blu"
               "brn"
               "gry"
               "grn"
               "hzl"
               "oth"})

(s/def ::pid (s/coll-of #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9} :count 9))
(s/def ::cid string?)

(s/def ::passport (s/keys :req [::byr ::iyr ::eyr ::hgt ::hcl ::ecl ::pid]
                          :opt [::cid]))


(s/exercise ::passport)


(defn parse-height [s]
  (when (re-matches #"^\d+(cm|in)$" s)
    (let [[_ v unit] (re-find #"^(\d+)(cm|in)$" s)]
      [(keyword unit) (Integer/parseInt v)])))

(defn parse-color [s]
  (when (re-matches #"^#[0-9a-f]{6}$" s)
    [(Integer/parseInt (subs s 1 3) 16)
     (Integer/parseInt (subs s 3 5) 16)
     (Integer/parseInt (subs s 5 7) 16)]))

(defn parse-int [s]
  (try
    (Integer/parseInt s)
    (catch NumberFormatException _
      nil)))

(defn parse [s]
  (let [tokens      (str/split s #" ")
        parse-token (fn [[k v]]
                      (case k
                        "byr" [::byr (parse-int v)]
                        "iyr" [::iyr (parse-int v)]
                        "eyr" [::eyr (parse-int v)]
                        "hgt" [::hgt (parse-height v)]
                        "hcl" [::hcl (parse-color v)]
                        "ecl" [::ecl v]
                        "pid" [::pid (seq v)]
                        "cid" [::cid v]
                        ))]
    (->> (map #(str/split % #":") tokens)
         (map parse-token)
         (into {}))))

(defn part2 [input]
  (->> (str/split-lines input)
       (map parse)
       (filter #(s/valid? ::passport %))
       (count)))

(comment
  (let [s "ecl:amb hgt:177cm hcl:#b6a3ce eyr:2025 byr:1967 pid:506927066 iyr:2018 cid:93 "
        s "pid:859849571 ecl:amb hcl:#6b5442 hgt:193cm byr:1980 iyr:2017 eyr:2020"]
    (s/valid? ::passport (parse s))
    (parse s)
    #_(when-not (s/valid? ::passport (parse s))
        (prn)
        (s/explain ::passport (parse s))))


  @(def input (aoc/input 2020 4))
  (part2 input)
  )