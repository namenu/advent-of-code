(ns aoc.graph
  (:require [clojure.data.priority-map :refer [priority-map]])
  (:import (clojure.lang PersistentQueue)))

(defn bfs [start adjacent]
  (loop [visited {start 0}
         queue   (conj PersistentQueue/EMPTY start)]
    (if-let [cur (peek queue)]
      (let [neighbors (->> (adjacent cur)
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

(defn A* [start goal? h neighbor+weight]
  (loop [g_map {start 0}
         queue (priority-map start (h start))]
    (if-let [[cur f_cur] (peek queue)]
      (if (goal? cur)
        [cur f_cur]
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
