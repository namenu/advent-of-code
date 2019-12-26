;; --- Day 18: Many-Worlds Interpretation ---
(ns aoc.year2019.day18
  (:require [aoc.util :refer [input]]
            [aoc.grid :refer [parse-grid adjacent-4 diagonal-4]]
            [aoc.graph :refer [bfs simple-A* A*]]
            [clojure.string :as str]))

(def wall? #(= \# %))
(def node? #(not (#{\# \.} %)))

(def matching-key (let [doors "ABCDEFGHIJKLMNOPQRSTUVWXYZ"]
                    (zipmap doors (str/lower-case doors))))
(def door? (set (keys matching-key)))
(def key? (set (vals matching-key)))

(defn edges-from [grid [start-pos start-v]]
  (let [adj-fn (fn [pos]
                 (if (and (not= pos start-pos) (node? (grid pos)))
                   []
                   (remove #(wall? (grid %)) (adjacent-4 pos))))
        edges  (->> (bfs start-pos adj-fn)
                    (reduce (fn [edges [pos dist]]
                              (let [v (grid pos)]
                                (if (node? v)
                                  (assoc edges v dist)
                                  edges))) {}))]
    (dissoc edges start-v)))

(defn build-graph [grid]
  (let [nodes (filter #(node? (second %)) grid)]
    (->> nodes
         (map #(vector (second %) (edges-from grid %)))
         (into {}))))

(defn find-edges [graph u]
  (map (fn [[v d]] [u v d]) (graph u)))


(defn part1 [in]
  (let [grid     (parse-grid in)
        graph    (build-graph grid)

        num-keys (count (filter key? (vals grid)))]
    (let [init       {:node    \@
                      :keyring #{}}
          goal?      #(= num-keys (count (:keyring %)))
          adj-fn     (fn [[{:keys [node keyring] :as cur} cost]]
                       (->> (find-edges graph node)
                            (filter (fn [[_ v _]]
                                      (if (door? v)
                                        (keyring (matching-key v))
                                        true)))
                            (map (fn [[_ v dist]]
                                   (let [new (cond-> (assoc cur :node v)
                                               (key? v) (update :keyring conj v))]
                                     [new (+ cost dist)])))))
          promising? (fn [[candidate cost] reach]
                       (if-let [prev-cost (reach candidate)]
                         (< cost prev-cost)
                         true))]
      (simple-A* init goal? adj-fn promising?))))

#_(time (part1 (input 2019 18)))


(defn part2 [grid]
  (let [graph    (build-graph grid)
        num-keys (count (filter key? (vals grid)))]
    (let [start       {:nodes   #{\@ \! \$ \%}
                       :keyring #{}}
          goal?       #(= num-keys (count (:keyring %)))
          ; fairly inaccurate estimation for speed. ¯\_(ツ)_/¯
          h           (fn [node]
                        (* 1 (- num-keys (count (:keyring node)))))
          neighbor-fn (fn [{:keys [nodes keyring] :as cur}]
                        (->> (mapcat #(find-edges graph %) nodes)
                             (filter (fn [[_ v _]]
                                       (if (door? v)
                                         (keyring (matching-key v))
                                         true)))
                             (map (fn [[u v dist]]
                                    (let [new (cond-> (assoc cur :nodes (-> nodes (disj u) (conj v)))
                                                (key? v) (update :keyring conj v))]
                                      [new dist])))))]
      (second (A* start goal? h neighbor-fn)))))

(defn split-grid [grid]
  (let [[start _] (first (filter #(= \@ (second %)) grid))]
    (merge (assoc grid start \#)
           (zipmap (adjacent-4 start) (repeat 4 \#))
           (zipmap (diagonal-4 start) [\@ \! \$ \%]))))

#_(time
    (part2 (-> (input 2019 18) (parse-grid) (split-grid))))
