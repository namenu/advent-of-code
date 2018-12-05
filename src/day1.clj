(ns day1)

(defn part1 [input]
  (reduce + (map Integer/parseInt input)))

(defn part2 [input]
  (loop [freq (cycle (map Integer/parseInt input))
         sum  0
         seen #{}]
    (let [f (+ sum (first freq))]
      (if (seen f)
        f
        (recur (next freq) f (conj seen f))))))
