(ns year2018.day25
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.java.io :as io]
            [util :refer [manhattan-dist]]))


(defn input->points [input]
  (->> (str/split-lines input)
       (map #(str "[" % "]"))
       (mapv read-string)))

(defn connected? [a b]
  (<= (util/manhattan-dist a b) 3))

(defn find-graph [v G]
  (loop [spanning [v]
         H        #{v}]
    (if-let [v (first spanning)]
      (let [connections (G v)]
        (recur (into (next spanning) (set/difference connections H))
               (set/union H connections)))
      H)))

(defn constellations [points]
  (let [E (for [i points
                j points
                :when (and (connected? i j))]
            [i j])
        G (reduce (fn [g [k v]]
                    (update g k (fnil conj #{}) v))
                  {}
                  E)
        V (set (keys G))]
    (loop [remaining V
           subgraphs []]
      (if-let [v (first remaining)]
        (let [H (find-graph v G)]
          (recur (set/difference remaining H)
                 (conj subgraphs H)))
        subgraphs))))

(def input1 "0,0,0,0\n 3,0,0,0\n 0,3,0,0\n 0,0,3,0\n 0,0,0,3\n 0,0,0,6\n 9,0,0,0\n12,0,0,0")
(def input2 "-1,2,2,0\n0,0,2,-2\n0,0,0,-2\n-1,2,0,0\n-2,-2,-2,2\n3,0,2,-1\n-1,3,2,2\n-1,0,-1,0\n0,2,1,-2\n3,0,0,0")
(def input3 "1,-1,0,1\n2,0,-1,0\n3,2,-1,0\n0,0,3,1\n0,0,-1,-1\n2,3,-2,0\n-2,2,0,0\n2,-2,0,-1\n1,-1,0,-1\n3,2,0,2\n")
(def input4 "1,-1,-1,-2\n-2,-2,0,1\n0,2,1,3\n-2,3,-2,1\n0,2,3,-2\n-1,-1,1,-2\n0,-2,-1,0\n-2,2,3,-1\n1,2,2,0\n-1,-2,0,-2")

; part1
(def input (-> "day25.in" io/resource slurp))
(count (constellations (input->points input)))
