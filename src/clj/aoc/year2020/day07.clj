(ns aoc.year2020.day07
  (:require [clojure.string :as str]
            [aoc.util :as aoc]
            [aoc.graph :as graph]))

(defn parse-rule
  "s => [light-red [[bright-white 1] [muted-yellow 2]]."
  [s]
  (let [[from tos] (str/split s #" contain ")
        from (str/replace from " bags" "")]
    (if (str/index-of tos "no")
      nil
      (let [parse-bag (fn [s]
                        (let [[_ n bag] (re-find #"(\d+) (.+) bag" s)]
                          [bag (Integer/parseInt n)]))]
        (->> (str/split tos #", ")
             (map #(apply vector from (parse-bag %))))))))

(defn parse-rules [input]
  (->> (str/split-lines input)
       (mapcat parse-rule)))

(defn part1 [input]
  (let [rules  (parse-rules input)
        verts  (->> rules
                    (mapcat (fn [[from to _]]
                              [from to]))
                    (into #{}))
        adj-fn (->> rules
                    (reduce (fn [res [from to _]]
                              (update res from (fnil conj []) to))
                            {}))]
    (->> verts
         (keep #((graph/bfs % adj-fn) "shiny gold"))
         (count)
         (dec))))


(comment
  (def sample "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")
  (def input (aoc/input 2020 7))

  (parse-rules input)
  (part1 sample)
  (time (part1 input))
  #_(part2 input))