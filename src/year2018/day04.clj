(ns year2018.day04
  (:require [clojure.string :as str]))

(defn parse-record [s]
  (let [[_ datetime msg] (re-find #"\[(.+)\] (.+)" s)
        record {:minute (->> (re-find #":(\d+)$" datetime) second Integer/parseInt)
                :type   (cond
                          (str/starts-with? msg "Guard") :shift
                          (str/starts-with? msg "falls") :asleep
                          (str/starts-with? msg "wakes") :awake)}]
    (if (= (:type record) :shift)
      (let [id (Integer/parseInt (re-find #"\d+" msg))]
        (assoc record :id id))
      record)))

(defn input->records [input]
  (->> (map parse-record (sort input))
       (reduce (fn [{:keys [state] :as acc} record]
                 (case (:type record)
                   :shift (assoc-in acc [:state :id] (:id record))
                   :asleep (assoc-in acc [:state :start] (:minute record))
                   :awake (update-in acc [:result (:id state)] conj [(:start state) (:minute record)])))
               {:result {} :state {}})
       :result))

(defn most-sleepy-minute [intervals]
  (let [stats (for [m (range 60)
                    :let [num-slept (->> intervals
                                         (filter (fn [[begin end]] (and (<= begin m) (< m end))))
                                         (count))]]
                [m num-slept])]
    (apply max-key second stats)))

(defn solve [key-fn records]
  (let [sleepy-guard (apply max-key key-fn records)
        [id intervals] sleepy-guard]
    (* id (first (most-sleepy-minute intervals)))))

(defn part1 [input]
  (let [key-fn #(reduce (fn [sum [begin end]] (+ sum (- end begin))) 0 (val %))]
    (solve key-fn (input->records input))))

(defn part2 [input]
  (let [key-fn #(second (most-sleepy-minute (val %)))]
    (solve key-fn (input->records input))))


;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test-day4
  (let [input ["[1518-11-01 00:00] Guard #10 begins shift"
               "[1518-11-01 00:05] falls asleep"
               "[1518-11-01 00:25] wakes up"
               "[1518-11-01 00:30] falls asleep"
               "[1518-11-01 00:55] wakes up"
               "[1518-11-01 23:58] Guard #99 begins shift"
               "[1518-11-02 00:40] falls asleep"
               "[1518-11-02 00:50] wakes up"
               "[1518-11-03 00:05] Guard #10 begins shift"
               "[1518-11-03 00:24] falls asleep"
               "[1518-11-03 00:29] wakes up"
               "[1518-11-04 00:02] Guard #99 begins shift"
               "[1518-11-04 00:36] falls asleep"
               "[1518-11-04 00:46] wakes up"
               "[1518-11-05 00:03] Guard #99 begins shift"
               "[1518-11-05 00:45] falls asleep"
               "[1518-11-05 00:55] wakes up"]]
    (is (= 240 (part1 input)))
    (is (= 4455 (part2 input)))))

(run-tests)





