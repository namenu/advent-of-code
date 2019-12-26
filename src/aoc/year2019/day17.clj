;; --- Day 17: Set and Forget ---
(ns aoc.year2019.day17
  (:require [aoc.util :refer [input]]
            [aoc.grid :refer [parse-grid adjacent-4]]
            [aoc.year2019.intcode :refer :all]
            [clojure.string :as str]))

; pt.1
(let [output   (-> (input 2019 17) (input->machine) (run) :output)
      grid-str (apply str (map char output))
      ;grid-str "..#..........\n..#..........\n#######...###\n#.#...#...#.#\n#############\n..#...#...#..\n..#####...^.."
      grid     (parse-grid grid-str)
      scaffold (->> (filter #(= \# (second %)) grid)
                    (map first)
                    (set))
      cross?   #(->> (adjacent-4 %)
                     (every? scaffold))]
  (->> (filter cross? scaffold)
       (map #(apply * %))
       (apply +)))


; pt.2
(comment
  (str/split-lines grid-str))

(let [cmd    (str "A,A,B,C,B,C,B,C,A,C\n"
                  "R,6,L,8,R,8\n"
                  "R,4,R,6,R,6,R,4,R,4\n"
                  "L,8,R,6,L,10,L,10\n"
                  ",y\n")
      feed   (map int cmd)
      robot  (input->machine (input 2019 17))
      robot' (reduce #(add-input %1 %2) robot feed)]
  (peek (:output (run robot'))))
