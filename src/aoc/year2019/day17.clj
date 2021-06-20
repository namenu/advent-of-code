;; --- Day 17: Set and Forget ---
(ns aoc.year2019.day17
  (:require [aoc.util :refer [input]]
            [aoc.grid :refer [parse-grid adjacent-4]]
            [aoc.year2019.intcode :refer :all]
            [clojure.string :as str]))

; pt.1
(let [[_ output] (-> (input->machine (input 2019 17))
                     (run)
                     (get-output {:ascii true}))
      ;output "..#..........\n..#..........\n#######...###\n#.#...#...#.#\n#############\n..#...#...#..\n..#####...^.."
      grid     (parse-grid output)
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

(let [cmd   (str "A,A,B,C,B,C,B,C,A,C\n"
                 "R,6,L,8,R,8\n"
                 "R,4,R,6,R,6,R,4,R,4\n"
                 "L,8,R,6,L,10,L,10\n"
                 ",y\n")
      robot (-> (input->machine (input 2019 17))
                (add-input cmd {:ascii true}))]
  (peek (:output (run robot))))
