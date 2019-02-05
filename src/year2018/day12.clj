(ns year2018.day12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [util :refer [first-duplicate]]))

(def plant? #(= \# %))

(defn parse-pattern [s]
  (let [[from to] (str/split s #" => ")]
    [(reduce (fn [acc c]
               (bit-or (bit-shift-left acc 1)
                       (if (plant? c) 1 0))) 0 from)
     (if (plant? (first to)) 1 0)]))

(defn parse-input [input]
  (let [[state _ & patterns] (str/split-lines input)
        [_ gen0] (re-find #"initial state: (.*)" state)]
    [{:pots gen0 :offset 0 :nth 0}
     (into {} (map parse-pattern patterns))]))

(defn evolve [patterns {:keys [pots] :as state}]
  (loop [pots      (str pots "    ")
         sig       0
         next-pots []]
    (if-let [cur (first pots)]
      (let [sig (-> sig
                    (bit-shift-left 1)
                    (bit-and 31)
                    (bit-or (if (plant? cur) 1 0)))
            c   (if (= (patterns sig) 1)
                  \#
                  \space)]
        (recur (next pots)
               sig
               (conj next-pots c)))

      (let [[l r] (split-with (complement plant?) next-pots)]
        (-> state
            (assoc :pots (str/trimr (apply str r)))
            (update :offset + (- (count l) 2))
            (update :nth inc))))))

(defn sum-pot-nums [{:keys [pots offset]}]
  (loop [pots pots idx offset sum 0]
    (if-let [c (first pots)]
      (recur (next pots) (inc idx) (+ sum (if (plant? c) idx 0)))
      sum)))

(defn part1 [input]
  (let [[init patterns] (parse-input input)
        generations (iterate (partial evolve patterns) init)]
    (sum-pot-nums (nth generations 20))))

(defn part2 [input]
  (let [[init patterns] (parse-input input)
        gen-repeating (first-duplicate
                        :pots
                        (take 200 (iterate (partial evolve patterns) init)))
        gen-50b       (-> gen-repeating
                          (update :offset + (- 50000000000 (:nth gen-repeating)))
                          (assoc :nth 50000000000))]
    (sum-pot-nums gen-50b)))


;; tests
(require '[clojure.test :refer [deftest testing is run-tests]])

(deftest test-day12
  (let [input "initial state: #..#.#..##......###...###\n\n...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"]
    (is (= 325 (time (part1 input)))))

  (let [input (->> "year2018/day12.in" io/resource slurp)]
    (is (= 2767 (time (part1 input))))
    (is (= 2650000001362 (time (part2 input))))))

(run-tests)
