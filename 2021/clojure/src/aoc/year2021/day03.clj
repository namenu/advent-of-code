(ns aoc.year2021.day03
  (:require [clojure.string :as str]))

(defn transpose [xs]
  (apply mapv vector xs))

(defn parse-bin [^String s]
  (Long/parseLong s 2))

(defn gamma-rate [diagnosis]
  (let [g (->> (transpose diagnosis)
               (map frequencies)
               (map #(apply max-key val %))
               (map first)
               (apply str))]
    (parse-bin g)))

(defn part1 [diagnosis n]
  (let [gamma   (gamma-rate diagnosis)
        epsilon (reduce bit-flip gamma (range n))]
    (* gamma epsilon)))


(defn oxygen-filter [i ds]
  (let [freq        (frequencies (map #(nth % i) ds))
        most-common (if (>= (freq \1) (freq \0)) \1 \0)]
    (filter #(= (nth % i) most-common) ds)))

(defn co2-filter [i ds]
  (let [freq         (frequencies (map #(nth % i) ds))
        least-common (if (<= (freq \0) (freq \1)) \0 \1)]
    (filter #(= (nth % i) least-common) ds)))

(defn part2 [diagnosis n]
  (let [rating (fn [filter]
                 (let [reducer (fn [ds i]
                                 (if (= (count ds) 1)
                                   (reduced ds)
                                   (filter i ds)))]
                   (-> (reduce reducer diagnosis (range n))
                       first
                       parse-bin)))]
    (* (rating oxygen-filter) (rating co2-filter))))

(let [input (slurp "resources/day03.in")
      lines (str/split input #"\n")
      n     (-> lines first count)]
  (prn (part1 lines n))
  (prn (part2 lines n)))

