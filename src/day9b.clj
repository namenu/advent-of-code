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
      (->> score-map (apply max-key val) (second))
      (let [[circle score] (place circle marble)
            player (mod marble num-players)]
        (recur circle (inc marble) (update score-map player (fnil + 0) score))))))

(comment
  (time (play 459 71790))
  "Elapsed time: 151.743266 msecs"
  (time (play 459 7179000))
  "Elapsed time: 11923.412965 msecs"
  )
