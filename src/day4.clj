(ns day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input (->> "day4.in" io/resource io/reader line-seq))


(defn ->record [s]
  (let [[_ datetime msg] (re-find #"\[(.+)\] (.+)" s)

        minute (->> (re-find #":(\d+)$" datetime) second Integer.)
        type   (cond
                 (str/starts-with? msg "Guard") :shift
                 (str/starts-with? msg "falls") :asleep
                 (str/starts-with? msg "wakes") :awake)
        record {:type   type
                :minute minute}
        ]
    (if (= type :shift)
      (let [id (read-string (re-find #"\d+" msg))]
        (assoc record :id id))
      record)))

(defn records-by-id [records]
  (loop [[r & records] records
         id  -1
         out []]
    (if r
      (if (= (:type r) :shift)
        (recur records (:id r) out)
        (recur records id (conj out (assoc r :id id))))
      (group-by :id out))))

(defn total-asleep [[_ records]]
  (reduce (fn [acc [begin end]]
            (+ acc (- (:minute end) (:minute begin))))
          0
          (partition 2 records)))

(defn slept-at [m records]
  (->> (partition 2 records)
       (filter (fn [[begin end]]
                 (and (<= (:minute begin) m)
                      (< m (:minute end)))))
       (count)))

(defn part1 [input]
  (let [all-records  (->> (sort input) (map ->record) records-by-id)
        sleepy-guard (apply max-key total-asleep all-records)
        most-minute  (->> (for [i (range 60)]
                            [i (slept-at i (sleepy-guard 1))])
                          (sort-by second)
                          (last)
                          (first))]
    (* (sleepy-guard 0) most-minute)))


(defn part2 [input]
  (let [all-records (->> (sort input) (map ->record) records-by-id)
        all-stats   (reduce-kv (fn [res id records]
                                 (let [days-slept (->> (for [i (range 60)]
                                                         [i (slept-at i records)])
                                                       (sort-by second)
                                                       (last))]
                                   (assoc res id days-slept)))
                               {}
                               all-records)]
    (let [[sleepy-id [sleepy-minute _]] (apply max-key (comp second val) all-stats)]
      (* sleepy-id sleepy-minute))))
