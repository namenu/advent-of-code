;; --- Day 10: Monitoring Station ---
(ns year2019.day10
  (:require [util :refer [input cart->polar]]
            [grid :refer [parse-grid]]
            [year2019.intcode :refer :all]
            [clojure.string :as str]))

; pt.1
(defn already-hit? [[dx dy] hits]
  (some (fn [[hx hy]]
          (and (= (neg? dx) (neg? hx))
               (= (neg? dy) (neg? hy))
               (= (* dx hy) (* dy hx)))) hits))

(defn hits [asteroids [cx cy]]
  (loop [targets asteroids
         hits    []]
    (if-let [[tx ty] (first targets)]
      (let [[dx dy] [(- tx cx) (- ty cy)]]
        (if (already-hit? [dx dy] hits)
          (recur (next targets) hits)
          (recur (next targets) (conj hits [dx dy]))))
      hits)))

(defn best-monitoring [asteroids]
  (->> asteroids
       (map #(vector % (count (hits asteroids %))))
       (apply max-key second)))

(defn ->ray [[cx cy] [x y]]
  (let [[dx dy] [(- x cx) (- y cy)]
        [r theta] (cart->polar [dx (- dy)])
        theta (- (/ Math/PI 2) theta)]
    {:xy     [x y]
     :angle  (if (neg? theta)
               (+ theta Math/PI Math/PI) theta)
     :length r}))


(let [in        ".#..#\n.....\n#####\n....#\n...##"
      in        "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####"
      in        "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.\n"
      in        ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#.."
      in        ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##"
      in        (input 2019 10)
      grid      (parse-grid in)
      asteroids (->> (parse-grid in)
                     (filter #(= (second %) \#))
                     (map first)
                     (into #{}))
      [station nseen] (best-monitoring asteroids)]
  ; pt.1
  (prn nseen)

  ; pt.2
  (let [sorted    (->> (disj asteroids station)
                       (map (partial ->ray station))
                       (group-by :angle)
                       (sort)
                       (map (comp #(sort-by :length %) second)))

        rounds    (->> sorted
                       (iterate #(remove nil? (map next %)))
                       (take-while not-empty))

        removals  (->> (mapcat #(map first %) rounds)
                       (map :xy))]
    (nth removals 199)))
