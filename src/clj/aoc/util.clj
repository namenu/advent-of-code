(ns aoc.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input [year day]
  (-> (format "year%d/day%02d.in" year day) io/resource slurp))

(defn input-lines [year day]
  (-> (format "year%d/day%02d.in" year day) io/resource io/reader line-seq))

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn find-first-index [pred coll]
  (->> (map-indexed vector coll)
       (find-first (comp pred second))
       (first)))

(defn first-duplicate
  ([xs]
   (first-duplicate identity xs))
  ([key-fn xs]
   (let [result (reduce (fn [seen x]
                          (let [k (key-fn x)]
                            (if (seen k)
                              (reduced x)
                              (conj seen k))))
                        #{} xs)]
     (if (set? result)
       nil
       result))))

(defn first-duplicate-index
  "Returns the first cycle found as `[start end]`. (length = end - start + 1)
  Also can be implemented through a combination of first-duplicate & find-first."
  [coll]
  (loop [[c & rest] coll
         seen {c 0}
         nth  1]
    (if-let [c' (first rest)]
      (if-let [prev (seen c')]
        [prev nth]
        (recur rest (assoc seen c' nth) (inc nth))))))

(defn countp [pred coll]
  (count (filter pred coll)))

(defn rsort-by [keyfn coll]
  (sort-by keyfn #(compare %2 %1) coll))

(defn fixed-point [f x]
  (reduce #(if (= %1 %2) (reduced %1) %2)
          (iterate f x)))

(defn iterate-n
  "get functional power of x. f^n(x)"
  [f n x]
  (first (drop n (iterate f x))))

(defn bounding-box [points]
  (let [cols    (apply map vector points)
        min-max (map #(apply (juxt min max) %) cols)]
    (apply map vector min-max)))

(defn range-incl
  ([end]
   (range (inc end)))
  ([start end]
   (range start (inc end)))
  ([start end step]
   (range start (inc end) step)))

(defn manhattan-dist [a b]
  (reduce + (map #(Math/abs ^Integer (- %1 %2)) a b)))

(defn cart->polar [[x y]]
  [(Math/sqrt (+ (* x x) (* y y))) (Math/atan2 y x)])


(defn ordered-pairs
  "For a given seq s, it generates all ordered pairs as a lazy-seq..

   (ordered-pairs (range 3))
   => ([0 1] [0 2] [1 1] [1 2] [2 2])
  "
  [s]
  (letfn [(f0 [[s-first & s-next :as s] [t-first & t-next]]
            (lazy-seq
              (if s-first
                (if t-first
                  (cons [s-first t-first] (f0 s t-next))
                  (f0 s-next (next s-next)))
                nil)))]
    (f0 s (next s))))

;; memoize recursive function with Y-combinator
(defmacro memoize-rec [form]
  (let [[_fn* fname params & body] form
        params-with-fname (vec (cons fname params))]
    `(let [f# (memoize (fn ~params-with-fname
                         (let [~fname (partial ~fname ~fname)] ~@body)))]
       (partial f# f#))))