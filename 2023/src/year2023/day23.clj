(ns year2023.day23
  (:require [clojure.string :as str])
  (:import (clojure.lang PersistentQueue)))

(defprotocol Graph
  (-vertices [g])
  (-neighbors [g v]))


(defn queue
  ([] (PersistentQueue/EMPTY))
  ([coll]
   (reduce conj PersistentQueue/EMPTY coll)))

;;
;; state := {:visisted #{}, :cur-v ,,,,}
(defn bfs [g start-v goal?]
  (loop [queue   (queue [{:visited #{start-v}, :cur-v start-v}])
         longest -1]
    (if (empty? queue)
      (dec longest)

      (let [state               (peek queue)
            queue               (pop queue)
            visitable-neighbors (->> (-neighbors g (:cur-v state))
                                     (remove (:visited state)))]
        (recur
          ;; queue
          (reduce (fn [q v]
                    (let [state' (-> state
                                     (update :visited conj v)
                                     (assoc :cur-v v))]
                      (conj q state')))
                  queue
                  visitable-neighbors)
          ;; longest
          (if (goal? state)
            (count (:visited state))
            longest))))))

(defrecord Island [m slope?]
  Graph
  (-vertices [g] (keys m))
  (-neighbors [_ [x y]]
    (let [candidates (if slope?
                       (case (m [x y])
                         \> [[(inc x) y]]
                         \< [[(dec x) y]]
                         \v [[x (inc y)]]
                         \^ [[x (dec y)]]
                         ;; \.
                         [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])
                       [[(dec x) y] [(inc x) y] [x (dec y)] [x (inc y)]])]
      (filter m candidates))))

(def input "#.#####################\n#.......#########...###\n#######.#########.#.###\n###.....#.>.>.###.#.###\n###v#####.#v#.###.#.###\n###.>...#.#.#.....#...#\n###v###.#.#.#########.#\n###...#.#.#.......#...#\n#####.#.#.#######.#.###\n#.....#.#.#.......#...#\n#.#####.#.#.#########v#\n#.#...#...#...###...>.#\n#.#.#v#######v###.###v#\n#...#.>.#...>.>.#.###.#\n#####v#.#.###v#.#.###.#\n#.....#...#...#.#.#...#\n#.#########.###.#.#.###\n#...###...#...#...#.###\n###.###.#.###v#####v###\n#...#...#.#.>.>.#.>.###\n#.###.###.#.###.#.#v###\n#.....###...###...#...#\n#####################.#")
(def input-large (slurp (clojure.java.io/resource "day23.in")))

(defn parse-input [input]
  (let [lines  (str/split-lines input)
        rows   (map-indexed
                 (fn [y row]
                   (map-indexed (fn [x c]
                                  [[x y] c])
                                row))
                 lines)
        coords (mapcat (fn [row]
                         (remove (fn [[_ c]]
                                   (= c \#)) row))
                       rows)]
    (into {} coords)))

(def start [1 0])
(defn find-goal [island]
  (apply max-key second (-vertices island)))

(defn find-junctions [island]
  (filter (fn [pos]
            (> (count (-neighbors island pos)) 2))
          (-vertices island)))

;; edges {[u v] -> w}

(defn find-edges [island vertices]
  (letfn [(dfs [u visited length goal?]
            (if (goal? u)
              [[u length]]
              (->> (-neighbors island u)
                   (remove visited)
                   (mapcat (fn [v]
                             (dfs v (conj visited v) (inc length) goal?))))))]
    (->> (for [u vertices]
           (let [goal? #(and (not= % u) (vertices %))]
             [u (into {} (dfs u #{u} 0 goal?))]))
         (into {}))))

(defn longest-path [g start goal visited]
  (if (= start goal)
    0
    (let [neighbors (->> (-neighbors g start)
                         (remove (fn [[v _]] (visited v))))]
      (reduce (fn [longest [v w]]
                (max longest
                     (+ w (longest-path g v goal (conj visited start)))))
              -9999999
              neighbors))))


(comment

  (def m
    ;(parse-input input) #_
    (parse-input input-large))

  ;; pt.1
  (time
    (let [island (Island. m true)
          goal?  #(= (:cur-v %) (find-goal island))]
      (bfs island start goal?)))

  ;; pt.2
  (time
    (let [island    (Island. m false)
          goal      (find-goal island)
          junctions (find-junctions island)
          vertices  (into #{start goal} junctions)
          edges     (find-edges island vertices)

          g         (reify Graph
                      (-vertices [_] vertices)
                      (-neighbors [_ v] (edges v)))]
      (longest-path g start goal #{})
      ))

  )