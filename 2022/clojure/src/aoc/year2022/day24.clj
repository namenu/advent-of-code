(ns aoc.year2022.day24
  (:require [aoc.grid :as grid]
            [aoc.graph :as graph]))

(defn parse-blizzard [[[x y] dir] [mod-x mod-y]]
  (let [x (dec x)
        y (dec y)]
    (case dir
      \< (fn [t] [(mod (- x t) mod-x), y])
      \> (fn [t] [(mod (+ x t) mod-x), y])
      \^ (fn [t] [x, (mod (- y t) mod-y)])
      \v (fn [t] [x, (mod (+ y t) mod-y)]))))

(defn parse-input [input]
  (let [g         (grid/read-grid input)
        bbox      (grid/bounding-box (map first g))
        mod-xy    (grid/fmap (second bbox) dec)
        blizzards (->> g
                       (filter #(#{\< \> \v \^} (second %)))
                       (map #(parse-blizzard % mod-xy)))]
    {:blizzards-at (memoize
                     (fn [t]
                       (->> blizzards
                            (map #(% t))
                            (into #{}))))
     :bbox         [[0 0] (grid/fmap mod-xy dec)]}))

(defrecord State [t pos])

(defn travel [state0 {:keys [bbox blizzards-at]} start goal]
  (let [blocked? (fn [{:keys [t pos]}]
                   ((blizzards-at t) pos))
        neighbor+weight
                 (fn [{:keys [t pos]}]
                   (let [candidates (->> (conj (grid/adjacent-4 pos) pos)
                                         (filter #(or (grid/inside? bbox %)
                                                      (= start %)
                                                      (= goal %)))
                                         (map #(->State (inc t) %))
                                         (remove blocked?))]
                     (map #(vector % 1) candidates)))]
    (-> (graph/A* state0
                  #(= (:pos %) goal)
                  (fn [{:keys [t pos]}]
                    (+ t (grid/manhattan-dist pos goal)))
                  neighbor+weight)
        :state)))

(defn part1 [input]
  (let [basin    (parse-input input)
        entrance [0 -1]
        goal     (let [[mx my] (second (:bbox basin))]
                   [mx (inc my)])]
    (travel (->State 0 entrance) basin entrance goal)))


(def input "#.#####\n#.....#\n#>....#\n#.....#\n#...v.#\n#.....#\n#####.#\n")
(def input "#.######\n#>>.<^<#\n#.<..<<#\n#>v.><>#\n#<^v^^>#\n######.#")
(def input (slurp (clojure.java.io/resource "day24.in")))

(defn part2 [input]
  (let [basin    (parse-input input)
        entrance [0 -1]
        goal     (let [[mx my] (second (:bbox basin))]
                   [mx (inc my)])]
    (-> (->State 0 entrance)
        (travel basin entrance goal)
        (travel basin goal entrance)
        (travel basin entrance goal))))

(parse-input input)
(part1 input)
(part2 input)
