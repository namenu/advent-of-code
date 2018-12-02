(ns main
  (:require [clojure.java.io :as io]))

(defn day1-1 [input]
  (reduce + input))

(defn day1-2 [input]
  (loop [freq (cycle input)
         sum  0
         seen #{}]
    (let [f (+ sum (first freq))]
      (if (seen f)
        f
        (recur (next freq) f (conj seen f))))))

(with-open [r (clojure.java.io/reader (io/resource "day1.in"))]
  (let [input (map read-string (line-seq r))]
    (day1-2 input)))



(defn fingerprint [id]
  (let [occurs (into #{} (vals (frequencies id)))]
    [(if (occurs 2) 1 0)
     (if (occurs 3) 1 0)]))

(defn day2-1 [input]
  (reduce * (reduce #(map + %1 %2) (map fingerprint input))))

(def input ["abcdef"
            "bababc"
            "abbcde"
            "abcccd"
            "aabcdd"
            "abcdee"
            "ababab"])


(def input2 ["abcde"
             "fghij"
             "klmno"
             "pqrst"
             "fguij"
             "axcye"
             "wvxyz"])

(defn diff [x y]
  (->> (map #(- (int %1) (int %2)) x y)
       (remove zero?)
       count))

(defn common-letters [x y]
  (apply str (map #(if (= %1 %2) %1 nil) x y)))

(defn day2-2 [input]
  (doseq [i input
          j input]
    (if (= (diff i j) 1)
      (prn (common-letters i j)))))

(with-open [r (clojure.java.io/reader (io/resource "day2.in"))]
  (let [input (line-seq r)]
    (day2-2 input)))
