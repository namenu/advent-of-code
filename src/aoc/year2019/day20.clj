;; --- Day 20: Donut Maze ---
(ns aoc.year2019.day20
  (:require [aoc.util :refer [input range-incl]]
            [aoc.grid :refer [parse-grid print-grid adjacent-4]]
            [aoc.graph :refer [bfs floyd A*]]
            [clojure.string :as str]))

(defn edge-points
  "[top bottom left right]"
  [[x y width height]]
  [(map vector (range x (+ x width)) (repeat y))
   (map vector (range x (+ x width)) (repeat (+ y height -1)))
   (map vector (repeat x) (range y (+ y height)))
   (map vector (repeat (+ x width -1)) (range y (+ y height)))])

(defn parse-portal [grid points [d1 d2]]
  (let [get-name #(str (grid (mapv + % d1)) (grid (mapv + % d2)))]
    (->> points
         (filter #(= (grid %) \.))
         (map #(vector (get-name %) %)))))

(defn parse-maze [in weight hole-size]
  (let [grid      (parse-grid in)
        [hx hy] hole-size
        outer-box [2 2 (+ weight weight hx) (+ weight weight hy)]
        inner-box [(+ 2 weight -1) (+ 2 weight -1) (+ hx 2) (+ hy 2)]]
    (let [outer-offsets [[[0 -2] [0 -1]]
                         [[0 1] [0 2]]
                         [[-2 0] [-1 0]]
                         [[1 0] [2 0]]]
          inner-offsets [[[0 1] [0 2]]
                         [[0 -2] [0 -1]]
                         [[1 0] [2 0]]
                         [[-2 0] [-1 0]]]
          outer-portals (mapcat #(parse-portal grid %1 %2) (edge-points outer-box) outer-offsets)
          inner-portals (mapcat #(parse-portal grid %1 %2) (edge-points inner-box) inner-offsets)]
      {:grid    (reduce-kv (fn [res k v]
                             (if (or (= \. v) (= \# v))
                               (assoc res k v)
                               res))
                           {}
                           grid)
       :portals (reduce (fn [res [k v]]
                          (assoc res (str/lower-case k) v))
                        (into {} outer-portals)
                        inner-portals)})))

(defn transpose [m]
  (reduce-kv #(assoc %1 %3 %2) {} m))

(defn find-edges [{:keys [grid portals]} [p1 start-pos]]
  (let [adj-fn     (fn [pos]
                     (filter #(= \. (grid %)) (adjacent-4 pos)))
        is-portal? (transpose portals)]
    (->> (bfs start-pos adj-fn)
         (reduce-kv (fn [edges pos dist]
                      (let [p2 (is-portal? pos)]
                        (if (and p2 (not= p1 p2))
                          (assoc edges [p1 p2] dist)
                          edges)))
                    {}))))

(defn build-edges [maze vertices]
  (let [edges     (->> (:portals maze)
                       (map #(find-edges maze %))
                       (apply merge))
        teleports (->> (disj vertices "AA" "ZZ")
                       (map (fn [u]
                              (let [v (if (Character/isUpperCase (first u))
                                        (str/lower-case u)
                                        (str/upper-case u))]
                                [[u v] 1])))
                       (into {}))]
    (into edges teleports)))

(let [in             "         A           \n         A           \n  #######.#########  \n  #######.........#  \n  #######.#######.#  \n  #######.#######.#  \n  #######.#######.#  \n  #####  B    ###.#  \nBC...##  C    ###.#  \n  ##.##       ###.#  \n  ##...DE  F  ###.#  \n  #####    G  ###.#  \n  #########.#####.#  \nDE..#######...###.#  \n  #.#########.###.#  \nFG..#########.....#  \n  ###########.#####  \n             Z       \n             Z       "
      maze           (parse-maze in 5 [7 5])

      in             (input 2019 20)
      maze           (parse-maze in 29 [53 59])

      vertices       (set (keys (:portals maze)))
      edges          (build-edges maze vertices)

      shortest-pairs (floyd vertices edges)
      ]
  ; pt.1
  (shortest-pairs ["AA" "ZZ"])

  ; pt.2
  (let [level-diff  (fn [u v]
                      (if (= 1 (edges [u v]))
                        (if (Character/isUpperCase (first v)) +1 -1)
                        0))
        start       ["AA" 0]
        goal?       #(= ["ZZ" 0] %)
        h           (fn [[node _]]
                      (if (= "ZZ" node)
                        0
                        (shortest-pairs [node "ZZ"])))
        neighbor-fn (fn [[node level]]
                      (->> (filter #(= (ffirst %) node) edges)
                           (map (fn [[[u v] d]]
                                  [[v (+ level (level-diff u v))] d]))
                           (filter (fn [[[_ lv] _]]
                                     (>= lv 0)))))]
    (A* start goal? h neighbor-fn)))
