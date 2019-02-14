;; --- Day 14: Chocolate Charts ---
(ns year2018.day14
  (:require [util :refer [find-first find-first-index]]))

(defn recipe-seq [a b]
  (letfn [(next-recipes [recipes sum]
            (if (< sum 10)
              (conj recipes sum)
              (conj recipes 1 (rem sum 10))))

          (step [i j recipes]
            (lazy-seq
              (let [x        (nth recipes i)
                    y        (nth recipes j)
                    sum      (+ x y)
                    recipes' (next-recipes recipes sum)
                    seq0     (cons (rem sum 10)
                                   (step (mod (+ i x 1) (count recipes'))
                                         (mod (+ j y 1) (count recipes'))
                                         recipes'))]
                (if (> sum 9)
                  (cons 1 seq0)
                  seq0))))]
    (cons a (cons b (step 0 1 [a b])))))

(defn pt1 [input]
  (->> (recipe-seq 3 7)
       (drop input)
       (take 10)))

(comment
  (pt1 9)
  (pt1 293801))

(defn pt2 [match]
  (->> (recipe-seq 3 7)
       (partition (count match) 1)
       (find-first-index #(= % match))))

(comment
  (pt2 [5 1 5 8 9])
  (pt2 [0 1 2 4 5])
  (pt2 [9 2 5 1 0])
  (pt2 [5 9 4 1 4])
  (time
    (pt2 [2 9 3 8 0 1])))
