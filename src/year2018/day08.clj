(ns year2018.day08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn tree-sum [data]
  (let [[num-child num-meta & rest] data]
    (loop [i    num-child
           acc  0
           data rest]
      (if (zero? i)
        [(apply + acc (take num-meta data)) (drop num-meta data)]
        (let [[child-acc next-data] (tree-sum data)]
          (recur (dec i) (+ acc child-acc) next-data))))))

(defn parse-tree [data]
  (let [[num-child num-meta & rest] data]
    (loop [i        num-child
           children []
           data     rest]
      (if (zero? i)
        [{:child children :meta (take num-meta data)} (drop num-meta data)]
        (let [[child next-data] (parse-tree data)]
          (recur (dec i) (conj children child) next-data))))))

(defn tree-sum-2 [{:keys [child meta]}]
  (if (empty? child)
    (reduce + meta)
    (let [indices (map dec (filter #(<= 1 % (count child)) meta))]
      (reduce + (map #(tree-sum-2 (nth child %)) indices)))))

(defn part1 [input]
  (let [data (map #(Integer/parseInt %) (str/split input #" "))]
    (first (tree-sum data))))

(defn part2 [input]
  (let [data (map #(Integer/parseInt %) (str/split input #" "))
        tree (first (parse-tree data))]
    (tree-sum-2 tree)))

;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day8
  (let [input "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"]
    (is (= 138 (part1 input)))
    (is (= 66 (part2 input))))

  (comment
    (require '[clojure.java.io :as io])
    (def input (->> "year2018/day08.in" io/resource io/reader line-seq)))
  )

(run-tests)