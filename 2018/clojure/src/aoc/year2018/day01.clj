(ns aoc.year2018.day01)

(defn part1 [input]
  (reduce + (map #(Integer/parseInt %) input)))

; TODO: reductions + reduced
(defn part2 [input]
  (loop [freq (cycle (map #(Integer/parseInt %) input))
         sum  0
         seen #{}]
    (let [f (+ sum (first freq))]
      (if (seen f)
        f
        (recur (next freq) f (conj seen f))))))


(def input "+3 +3 +4 -2 -4")

(defn first-duplicate
  "xs의 원소는 nil 혹은 set이 아니어야 함"
  [xs]
  (let [r (reduce (fn [seen? x]
                    (if (seen? x)
                      (reduced x)
                      (conj seen? x)))
                  #{} xs)]
    (if-not (set? r)
      r)))

(first-duplicate (reductions + (cycle input)))

(let [ns (map #(Integer/parseInt %) (clojure.string/split input #" "))]
  (comment
    (first-duplicate (reductions + (cycle input))))

    (first-duplicate [1 2 3 4 1])
    (first-duplicate [1 2 3 4 10]))
