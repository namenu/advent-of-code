(ns day5
  (:require [clojure.string :as str]))

(def lower "abcdefghijklmnopqrstuvwxyz")
(def upper "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def react-pattern (re-pattern (str/join "|" (map str (str lower upper) (str upper lower)))))

(defn collapse [polymer pattern]
  (str/replace polymer pattern ""))

(defn collapse-length [polymer]
  (loop [polymer polymer]
    (let [destroyed (str/replace polymer react-pattern "")]
      (if (= (count polymer) (count destroyed))
        (count polymer)
        (recur destroyed)))))

(defn part1 [input]
  (collapse-length input))

(defn part2 [input]
  (let [removing (map (comp re-pattern str) lower (repeat "|") upper)
        removed  (map (partial collapse input) removing)]
    (apply min (map collapse-length removed))))


(comment
  (require '[clojure.java.io :as io])

  (def input "dabAcCaCBAcCcaDA")
  (def input (->> "day5.in" io/resource slurp))

  )