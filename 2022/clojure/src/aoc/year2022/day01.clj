(ns aoc.year2022.day01
  (:require [clojure.string :as str]))

(def input "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000\n")

@(def input (slurp (clojure.java.io/resource "day01.in")))

(->> (str/split input #"\n\n")
     (map str/split-lines)
     (map #(map parse-long %))
     (map #(apply + %))
     (apply max))