(ns year2017.day02
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [util :refer [find-first]]))

(def lines (str/split-lines "5 1 9 5\n7 5 3\n2 4 6 8"))
(def lines (line-seq (io/reader (io/resource "year2017/day02.in"))))

(defn parse-line [line]
  (->> (str/split line #"\s")
       (map #(Integer/parseInt %))))

(defn max-diff [numbers]
  (let [[a b] (apply (juxt min max) numbers)]
    (- b a)))

(def data (map parse-line lines))

; pt.1
(->> data
     (map max-diff)
     (apply +))


; pt.2
(defn even-division [numbers]
  (let [domain     (for [x numbers y numbers
                         :when (not= x y)]
                     [x y])
        divisible? (fn [[n d]] (zero? (mod n d)))]
    (apply quot (find-first divisible? domain))))

(->> (map even-division data)
     (apply +))