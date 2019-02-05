(ns year2018.day18
  (:require [util :refer [find-cycle]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (str/split-lines ".#.#...|#.\n.....#|##|\n.|..|...#.\n..|#.....#\n#.#|||#|#|\n...#.||...\n.|....|...\n||...#|.#|\n|.||||..|.\n...#.|..|."))
(def input (->> (-> "year2018/day18.in" io/resource io/reader line-seq)))

(def state (->> (for [[y line] (map-indexed vector input)
                      [x c] (map-indexed vector line)]
                  [[x y] c])
                (into {})))

(defn add [[x1 y1] [x2 y2]]
  [(+ x1 x2) (+ y1 y2)])

(defn adjacent [state pos]
  (->> [[-1 -1] [-1 0] [-1 1] [0 -1] [0 1] [1 -1] [1 0] [1 1]]
       (mapv add (repeat pos))
       (map state)
       (frequencies)))

(defn size [state]
  (let [coords (keys state)]
    [(inc (apply max (map first coords)))
     (inc (apply max (map second coords)))]))

(defn print-state [state]
  (let [[max-x max-y] (size state)]
    (doseq [y (range 0 max-y)]
      (doseq [x (range 0 max-x)]
        (print (state [x y])))
      (println)
      )))

(defn update-state [state]
  (let [[max-x max-y] (size state)]
    (reduce (fn [m pos]
              (let [c   (state pos)
                    adj (adjacent state pos)
                    c'  (case c
                          \. (if (>= (or (adj \|) 0) 3) \| \.)
                          \| (if (>= (or (adj \#) 0) 3) \# \|)
                          \# (if (and (>= (or (adj \#) 0) 1)
                                      (>= (or (adj \|) 0) 1)) \# \.))]
                (assoc m pos c'))
              )
            {}
            (for [y (range 0 max-y)
                  x (range 0 max-x)]
              [x y]))))


(defn resource-value [state minute]
  (let [state' (->> (nth (iterate update-state state) minute)
                    (map val)
                    (frequencies))]
    (* (state' \|)
       (state' \#))))


;part1
(resource-value state 10)


;part2
(let [[n m] (find-cycle (iterate update-state state))
      len (- m n)
      nth (+ n (mod (- 1000000000 n) len))]
  (resource-value state nth))
