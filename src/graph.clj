(ns graph
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

(defn A* [init adjacent promising?]
  (loop [reach {init 0}
         queue (priority-map init 0)]
    (if-let [cur (peek queue)]
      (let [candidates (->> (adjacent cur)
                            (filter #(promising? % reach)))]
        (recur (into reach candidates) (into (pop queue) candidates)))
      reach)))