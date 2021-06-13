(ns aoc.graph
  (:require [clojure.data.priority-map :refer [priority-map]])
  (:import (clojure.lang PersistentQueue)))


(defprotocol IDirectedGraph
  (edges [g] "Returns all edges #{[u v]*}")
  (vertices [g] "Returns all vertices #{v*}")

  (remove-vertex [g v]))

(defn topological-tops
  [^aoc.graph.IDirectedGraph g]
  (->> (map second (edges g))
       (apply disj (vertices g))))


(defn bfs
  "adjacent-fn :: pos -> [pos]
  Only works for unweighted graph."
  [start adjacent-fn]
  (loop [visited {start 0}
         queue   (conj PersistentQueue/EMPTY start)]
    (if-let [cur (peek queue)]
      (let [neighbors (->> (adjacent-fn cur)
                           (remove visited))
            dist      (inc (visited cur))
            visited'  (reduce (fn [visited cur] (assoc visited cur dist)) visited neighbors)]
        (recur visited' (into (pop queue) neighbors)))
      visited)))

(defn simple-A* [init goal? adjacent promising?]
  (loop [reach {init 0}
         queue (priority-map init 0)]
    (if-let [cur (peek queue)]
      (if (goal? (first cur))
        (second cur)
        (let [candidates (->> (adjacent cur)
                              (filter #(promising? % reach)))]
          (recur (into reach candidates) (into (pop queue) candidates))))
      reach)))

(defn A*
  "goal? :: node -> bool
   neighbor+weight :: node -> List [neighbor weight]
  `h` is a heuristic function that estimates the cost of the cheapest path from n to the goal."
  [start goal? h neighbor+weight]
  (loop [g_map {start 0}
         queue (priority-map start (h start))]
    (if-let [[cur f_cur] (peek queue)]
      (if (goal? cur)
        f_cur
        (let [g_cur (g_map cur)
              [g_map' queue'] (loop [[n+w & next] (neighbor+weight cur)
                                     g_map' g_map
                                     queue' (pop queue)]
                                (if-let [[neighbor weight] n+w]
                                  (let [tentative_g (+ g_cur weight)]
                                    (if (< tentative_g (or (g_map' neighbor) Integer/MAX_VALUE))
                                      (recur next
                                             (assoc g_map' neighbor tentative_g)
                                             (assoc queue' neighbor (+ tentative_g (h neighbor))))
                                      (recur next g_map' queue')))
                                  [g_map' queue']))]
          (recur g_map' queue')))
      'FAIL)))

(defn floyd [vertices edges]
  (let [update-ij (fn [d new-d]
                    (if (or (nil? d) (< new-d d))
                      new-d
                      d))]
    (reduce (fn [table k]
              (reduce (fn [table [i j new-d]]
                        (update table [i j] update-ij new-d))
                      table
                      (for [i vertices
                            j vertices
                            :let [u (table [i k])
                                  v (table [k j])]
                            :when (and (not= i j) u v)]
                        [i j (+ u v)])))
            edges
            vertices)))
