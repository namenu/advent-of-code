(ns day9b
  (:require [clojure.data.finger-tree :refer [double-list conjl]]))

(defn ccw [circle]
  (let [v (peek circle)]
    (-> circle (pop) (conjl v))))

(defn cw [circle]
  (let [v (first circle)]
    (-> circle (rest) (conj v))))

(defn place [circle next-num]
  (if (zero? (mod next-num 23))
    (let [circle   (nth (iterate ccw circle) 7)
          score-at (peek circle)]
      [(-> circle (pop) (cw)) (+ score-at next-num)])
    [(-> circle (cw) (conj next-num)) 0]))

(defn play [num-players last-marble]
  (loop [circle    (double-list 0)
         marble    1
         score-map {}]
    (if (> marble last-marble)
      score-map
      (let [player (mod marble num-players)
            player (if (= 0 player) num-players player)]
        (let [[circle score] (place circle marble)
              score-map (update score-map player (fnil + 0) score)]
          (recur circle (inc marble) score-map))))))

(comment
  ;459 71790
  (time
    (apply max-key val (play 459 7179000))))
