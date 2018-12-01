(ns main
  (:require [clojure.java.io :as io]))


; 1-1
(with-open [r (clojure.java.io/reader (io/resource "input.in"))]
  (let [input (map read-string (line-seq r))]
    (reduce + input)))


; 1-2
(with-open [r (clojure.java.io/reader (io/resource "input.in"))]
  (let [input (map read-string (line-seq r))]

    (loop [freq (cycle input)
           sum  0
           seen #{}]
      (let [f (+ sum (first freq))]
        (if (seen f)
          f
          (recur (next freq) f (conj seen f)))))))
