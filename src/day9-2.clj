(ns day9
  (:require [double-list :as dl]))

(defn vec-remove-at [v i]
  (vec (concat (subvec v 0 i) (subvec v (inc i)))))

@(def circle (dl/double-list (range 10)))
@(def current (dl/get-head circle))

(let [new-current (cw current)
      c2          (dl/add-after circle new-current 11)]
  (prn c2)
  (prn (dl/get-next (dl/get-prev new-current)))

  [c2 (dl/get-tail c2)]

  )


(defn add+current [circle current data]
  (let [next-key    (:key (dl/get-next current))
        new-circle  (dl/add-after circle current data)
        cur-key     (:key current)
        new-current (dl/get-prev (assoc (get (.m new-circle) next-key) :key next-key))]
    (prn (dl/get-next (assoc (get (.m new-circle) cur-key) :key cur-key)))
    [new-circle new-current]))

(add+current circle current 100)

(defn remove+current [circle current]
  (let [new-circle  (dl/remove-node circle current)
        new-current (if-let [next-key (:key (dl/get-next current))]
                      (assoc (get (.m new-circle) next-key) :key next-key)
                      (dl/get-head new-circle))]
    [new-circle new-current]))

(defn place [circle current next-num]
  (let [n (count circle)]
    (if (zero? (mod next-num 23))
      (let [to-remove (nth (iterate ccw current) 7)
            score-at  (:data to-remove)]
        (prn (conj (remove+current circle to-remove) (+ score-at next-num)))
        (conj (remove+current circle to-remove) (+ score-at next-num)))

      (let [next-current (or (dl/get-next current) (dl/get-head circle))]
        (if (= next-current (dl/get-tail circle))
          (let [new-circle  (dl/add-after circle next-current next-num)
                new-current (dl/get-tail new-circle)]
            (prn "2-1")
            [new-circle new-current 0])

          (let []
            (prn "2-2")
            (conj (add+current circle next-current next-num) 0))))
      #_(let [next-idx (mod (inc idx) n)]
          (if (= next-idx (- n 1))
            [(conj! circle next-num) (inc next-idx) 0]
            (let [[a b] (split-at (inc next-idx) circle)
                  v (vec (concat a (cons next-num b)))]
              [v (inc next-idx) 0])
            )))))

(defn play [num-players last-marble])
(def num-players 9)
(def last-marble 10)
(loop [circle    (dl/double-list (range 1))
       current   (dl/get-head circle)
       marble    1
       score-map {}]
  ;(do (prn marble))
  (prn circle (:data current))
  (if (> marble last-marble)
    score-map
    (let [player (mod marble num-players)
          player (if (= 0 player) num-players player)]
      (let [[circle current score] (place circle current marble)
            score-map (update score-map player (fnil + 0) score)]
        #_(prn (str "[" player "]") circle current)
        #_(when (pos? score)
            (prn "player" player "scores" score))
        (recur circle current (inc marble) score-map)))))

(comment
  ;459 71790
  (time
    (apply max-key val (play 459 5000))))

