(ns day9)

(defn vec-remove-at [v i]
  (vec (concat (subvec v 0 i) (subvec v (inc i)))))

(defn place [circle idx next-num]
  (let [n (count circle)]
    (if (zero? (mod next-num 23))
      (let [next-idx (mod (- idx 7) n)
            score-at (nth circle next-idx)]
        [(vec-remove-at circle next-idx) next-idx (+ score-at next-num)])
      (let [next-idx (mod (inc idx) n)]
        (if (= next-idx (- n 1))
          [(conj! circle next-num) (inc next-idx) 0]
          (let [[a b] (split-at (inc next-idx) circle)
                v (vec (concat a (cons next-num b)))]
            [v (inc next-idx) 0])
          )))))


(defn play [num-players last-marble]
  (loop [circle    (transient [0])                          ;[0 16 8 17 4 18 9 19 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15] ;[0]
         current   0                                        ; 13
         marble    1
         score-map {}]
    (do (prn marble))
    (if (> marble last-marble)
      score-map
      (let [player (mod marble num-players)
            player (if (= 0 player) num-players player)]
        (let [[circle current score] (place circle current marble)
              score-map (update score-map player (fnil + 0) score)]
          #_(prn (str "[" player "]") circle current)
          #_(when (pos? score)
              (prn "player" player "scores" score))
          (recur circle current (inc marble) score-map))))))


(subvec (persistent! (transient [0 1 2 3])) 1)
(conj! (transient [0 1 2 3]) 1)
(place (transient [0]) 0 23)

;459 71790

(time
  (apply max-key val (play 459 5000)))