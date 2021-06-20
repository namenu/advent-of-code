;; --- Day 6: Universal Orbit Map ---
(ns aoc.year2019.day06
  (:require [aoc.util :refer [input]]
            [clojure.string :as str]
            [clojure.set :as set]))

(defn input->orbits [in]
  (->> (str/split-lines in)
       (map (fn [line]
              (let [[a b] (str/split line #"\)")]
                [b a])))
       (into {})))

(defn path-to-com [orbits planet]
  (->> (iterate orbits planet)
       (take-while some?)
       (rest)))

(let [in      "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"
      in      "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nI)SAN\nK)YOU\n"
      in      (input 2019 6)
      orbits  (input->orbits in)
      planets (keys orbits)]

  (->> planets
       (map (partial path-to-com orbits))
       (map count)
       (apply +))

  (let [p1 (set (path-to-com orbits "YOU"))
        p2 (set (path-to-com orbits "SAN"))]
    (set/difference (set/union p1 p2) (set/intersection p1 p2))))
