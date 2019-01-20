(ns year2018.day06
  (:require [util :refer [manhattan-dist bounding-box]]))

(defn parse [s]
  (let [[_ x y] (re-find #"(\d+), (\d+)" s)]
    [(Integer/parseInt x) (Integer/parseInt y)]))

(defn closest-point [[x y] points]
  (let [[[c1 d1] [_ d2]] (->> points
                              (map #(vector % (manhattan-dist [x y] %)))
                              (sort-by second))]
    (when-not (= d1 d2)
      c1)))

(defn points-in-box [[[min-x min-y] [max-x max-y]]]
  (for [x (range min-x (inc max-x))
        y (range min-y (inc max-y))]
    [x y]))

(defn infinite-points
  "Points closest from the edges are spanning infinitely."
  [[[min-x min-y] [max-x max-y]] grid]
  (let [edge-points (concat (mapcat #(vector [% min-y] [% max-y]) (range min-x (inc max-x)))
                            (mapcat #(vector [min-x %] [max-x %]) (range min-y (inc max-y))))]
    (into #{} (remove nil? (map grid edge-points)))))

(defn part1
  "assume no duplications"
  [input]
  (let [points     (mapv parse input)
        bbox       (bounding-box points)
        distmap    (->> (points-in-box bbox)
                        (reduce #(let [c (closest-point %2 points)]
                                   (if c
                                     (conj %1 [%2 c])
                                     %1))
                                {}))
        areamap    (reduce #(update %1 (val %2) (fnil inc 0)) {} distmap)
        inf-point? (infinite-points bbox distmap)]
    (->> areamap
         (remove (fn [[k _]] (inf-point? k)))
         (apply max-key second)
         second)))

(defn sum-dists [p points]
  (reduce #(+ %1 (manhattan-dist p %2)) 0 points))

(defn part2
  "assumes that points outside of bbox always exceed limit"
  [input limit]
  (let [points (mapv parse input)
        bbox   (bounding-box points)]
    (->> (points-in-box bbox)
         (map #(sum-dists % points))
         (filter #(< % limit))
         (count))))


;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day6
  (let [input ["1, 1"
               "1, 6"
               "8, 3"
               "3, 4"
               "5, 5"
               "8, 9"]]
    (is (= 17 (part1 input)))
    (is (= 16 (part2 input 32))))

  (comment
    (require '[clojure.java.io :as io])
    (let [input (->> "day06.in" io/resource io/reader line-seq)]))
  )

(run-tests)
