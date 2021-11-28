;; --- Day 4: Secure Container ---
(ns aoc.year2019.day04
  (:require [aoc.util :refer [range-incl]]
            [clojure.spec.alpha :as s]))

(defn number->digits [n]
  (->> (iterate #(quot % 10) n)
       (take-while pos?)
       (map #(rem % 10))
       (into '())))

(s/def ::length #(= (count %) 6))

(s/def ::increasing? #(apply <= %))

(s/def ::adjacent-two
  (fn [digits]
    (not= (count digits) (count (dedupe digits)))))

(s/def ::part-1 (s/and ::length ::increasing? ::adjacent-two))

(s/def ::adjacent-exact-two
  (fn [digits]
    (->> (frequencies digits)
         (some #(= 2 (second %))))))

(s/def ::part-2 (s/and ::part-1 ::adjacent-exact-two))

(let [numbers (range-incl 356261 846303)]
  ; pt.1
  (prn
    (->> (map number->digits numbers)
         (filter #(s/valid? ::part-1 %))
         (count)))

  ; pt.2
  (prn
    (->> (map number->digits numbers)
         (filter #(s/valid? ::part-2 %))
         (count))))
