;; --- Day 24: Planet of Discord ---
(ns year2019.day24
  (:require [util :refer [input find-cycle]]
            [grid :refer [parse-grid print-grid adjacent-4]]))

(def bug? #(= \# %))

(defn next-minute [grid]
  (reduce-kv (fn [g pos v]
               (let [nbugs (count (filter #(bug? (grid %)) (adjacent-4 pos)))
                     next  (if (bug? v)
                             (if (= nbugs 1) \# \.)
                             (if (or (= nbugs 1) (= nbugs 2)) \# \.))]
                 (assoc g pos next)))
             {} grid))


(defn grid->score [grid]
  (->> (filter #(bug? (second %)) grid)
       (map (fn [[[x y] _]]
              (let [p (+ (* y 5) x)]
                (long (Math/pow 2 p)))))
       (apply +)))

; pt.1
(let [grid (parse-grid "....#\n#..#.\n#..##\n..#..\n#....")
      grid (parse-grid (input 2019 24))]
  (prn (->> (iterate next-minute grid)
            (find-cycle)))
  ; => 39
  (let [g (->> (iterate next-minute grid)
               (drop 39)
               (first))]
    (grid->score g)))


; pt.2
(defn evolve [grid inner-grid outer-grid]
  (let [top-edge    [[0 0] [1 0] [2 0] [3 0] [4 0]]
        bottom-edge [[0 4] [1 4] [2 4] [3 4] [4 4]]
        left-edge   [[0 0] [0 1] [0 2] [0 3] [0 4]]
        right-edge  [[4 0] [4 1] [4 2] [4 3] [4 4]]
        counter     (fn [[px py]]
                      (->> (adjacent-4 [px py])
                           (mapcat (fn [[x y]]
                                     (cond
                                       (< x 0) [(outer-grid [1 2])]
                                       (> x 4) [(outer-grid [3 2])]
                                       (< y 0) [(outer-grid [2 1])]
                                       (> y 4) [(outer-grid [2 3])]
                                       (and (= x 2) (= y 2))
                                       (cond
                                         (= py 1) (map inner-grid top-edge)
                                         (= py 3) (map inner-grid bottom-edge)
                                         (= px 1) (map inner-grid left-edge)
                                         (= px 3) (map inner-grid right-edge))
                                       :else [(grid [x y])])))
                           (filter bug?)
                           (count)))]
    (reduce-kv (fn [g pos v]
                 (let [nbugs (counter pos)
                       next  (if (bug? v)
                               (if (= nbugs 1) \# \.)
                               (if (or (= nbugs 1) (= nbugs 2)) \# \.))]
                   (assoc g pos next)))
               {}
               grid)))

(def grid-at-0-0
  (let [grid (parse-grid "....#\n#..#.\n#.?##\n..#..\n#....\n")
        grid (parse-grid (input 2019 24))]
    (dissoc grid [2 2])))

(def empty-grid (zipmap (keys grid-at-0-0) (repeat \.)))

(def grid-at
  (memoize
    (fn [level minute]
      (if (zero? minute)
        (if (zero? level)
          grid-at-0-0
          empty-grid)
        (evolve (grid-at level (dec minute))
                (grid-at (inc level) (dec minute))
                (grid-at (dec level) (dec minute)))))))

(let [minute 200]
  (->> (map #(grid-at % minute) (range -100 101))
       (mapcat #(filter bug? (vals %)))
       (count)))
