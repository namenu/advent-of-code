(ns day9)

(defn place [circle next-num]
  (if (zero? (mod next-num 23))
    (do
      (dotimes [_ 7]
        (.addFirst circle (.removeLast circle)))

      (let [score-at (.removeLast circle)]
        (.addLast circle (.removeFirst circle))
        [circle (+ score-at next-num)]))
    (do
      (let [front (.removeFirst circle)]
        (.addLast circle front)
        (.addLast circle next-num)
        [circle 0]))))


(defn play [num-players last-marble]
  (loop [circle    (java.util.ArrayDeque. [0])
         marble    1
         score-map {}]
    (if (> marble last-marble)
      score-map
      (let [player (mod marble num-players)
            player (if (= 0 player) num-players player)]
        (let [[circle score] (place circle marble)
              score-map (update score-map player (fnil + 0) score)]
          (recur circle (inc marble) score-map))))))


(apply max-key val (play 9 25))


;459 71790
(time
  (apply max-key val (play 459 7179000)))
