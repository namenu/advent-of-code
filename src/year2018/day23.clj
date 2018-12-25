(ns year2018.day23
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [bron-kerbosch :refer [maximum-cliques]]
            [util :refer [manhattan-dist range-incl]]))


(defn parse-line [s]
  (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" s)]
    {:pos (mapv #(Integer/parseInt %) [x y z])
     :r   (Integer/parseInt r)}))

(defn part1 [bots]
  (let [largest (apply max-key :r bots)]
    (->> bots
         (filter #(<= (manhattan-dist (:pos largest) (:pos %)) (:r largest)))
         (count))))


; part2
(comment
  (def input "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1\n")
  (def input (-> "day23.in" io/resource slurp))

  (def bots
    (map parse-line (str/split-lines input)))

  (def input "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5")

  (defn overlap? [a b]
    (<= (manhattan-dist (:pos a) (:pos b))
        (+ (:r a) (:r b))))

  (let [g          (reduce (fn [g [k v]]
                             (update g k (fnil conj #{}) v))
                           {}
                           (for [i bots
                                 j bots
                                 :when (and (not= i j) (overlap? i j))]
                             [i j]))

        ; find max-clique using Bron-Kerbosch algorithm
        ; [!] may not halt with this implementations...
        max-clique (apply max-key count (maximum-cliques (keys g) g))]
    (apply max (map #(- (manhattan-dist (:pos %) [0 0 0]) (:r %)) max-clique)))

  )

