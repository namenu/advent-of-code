(ns year2018.day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))


(def input "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #")
(def input (->> "day12.in" io/resource slurp))

(def plant? #(= \# %))

(defn parse-patterns [s]
  (let [[from to] (str/split s #" => ")]
    [(reduce (fn [acc c]
               (bit-or (bit-shift-left acc 1)
                       (if (plant? c) 1 0))) 0 from)
     (if (plant? (first to)) 1 0)]))

(defn parse-input [input]
  (let [[state _ & patterns] (str/split-lines input)
        [_ gen0] (re-find #"initial state: (.*)" state)]
    [gen0 (into {} (map parse-patterns patterns))]))


(defn evolve [patterns [gen leftmost]]
  (loop [gen      (str gen "    ")
         sig      0
         next-gen []]
    (if-let [cur (first gen)]
      (let [sig (-> sig
                    (bit-shift-left 1)
                    (bit-and 31)
                    (bit-or (if (plant? cur) 1 0)))
            c   (if (= (patterns sig) 1)
                  \#
                  \space)]
        (recur (next gen)
               sig
               (conj next-gen c)))

      (let [[l r] (split-with (complement plant?) next-gen)]
        [(str/trimr (apply str r))
         (+ leftmost (- (count l) 2))])
      )))


(let [[gen0 patterns] (parse-input input)]
  (loop [[gen idx] (nth (iterate (partial evolve patterns) [gen0 0]) 50000000000)
         sum 0]
    (if-let [c (first gen)]
      (recur [(next gen) (inc idx)]
             (+ sum (if (plant? c) idx 0)))
      sum)))
