;; --- Day 4: High-Entropy Passphrases ---
(ns year2017.day04
  (:require [util :refer [first-duplicate]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (-> "year2017/day04.in" io/resource slurp))

;; pt.1
(->> (str/split-lines input)
     (map #(str/split % #"\s"))
     (remove first-duplicate)
     (count))

;; pt.2
(->> (str/split-lines input)
     (map #(map sort (str/split % #"\s")))
     (remove first-duplicate)
     (count))
