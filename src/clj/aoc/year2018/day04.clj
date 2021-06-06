(ns aoc.year2018.day04
  (:require [clojure.string :as str])
  (:import (java.time.format DateTimeFormatter)
           (java.time LocalDateTime)))

(def parse-timestamp
  (let [fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd HH:mm")]
    (fn [s]
      (LocalDateTime/parse s fmt))))

(defn parse-record [s]
  (let [[_ ts msg] (re-find #"\[(.+)\] (.+)" s)
        record {:timestamp (parse-timestamp ts)
                :type      (cond
                             (str/starts-with? msg "Guard") :shift
                             (str/starts-with? msg "falls") :asleep
                             (str/starts-with? msg "wakes") :awake)}]
    (if (= (:type record) :shift)
      (let [id (Integer/parseInt (re-find #"\d+" msg))]
        (assoc record :id id))
      record)))

#_(defn group-by-guard
  [records]
  (->> records
       (partition-by :id)
       (partition 2)
       (map (fn [[[first] rest]]
              (let [id (:id first)]
                {id (partition 2 (mapv :minute rest))})))
       (apply merge-with concat)))

(defn aggregate [records]
  (->> records
       (reduce (fn [{:keys [state] :as acc} record]
                 (let [minute (.getMinute (:timestamp record))]
                   (case (:type record)
                     :shift (assoc-in acc [:state :id] (:id record))
                     :asleep (assoc-in acc [:state :start] minute)
                     :awake (update-in acc [:result (:id state)] conj [(:start state) minute]))))
               {:result {} :state {}})
       :result))

(defn input->data
  "example:
   {10 ([24 29] [30 55] [5 25]),
    99 ([45 55] [36 46] [40 50])}"
  [input]
  (->> input
       (map parse-record)
       (sort-by :timestamp #(.compareTo %1 %2))
       aggregate
       #_group-by-guard))

(defn most-sleepy-minute
  "가장 빈번하게 잠든 분을 구함.
  guard-info => {:minute :count}"
  [[_id intervals]]
  (let [stats (for [m (range 60)
                    :let [num-slept (->> intervals
                                         (filter (fn [[begin end]] (and (<= begin m) (< m end))))
                                         (count))]]
                {:minute m
                 :count  num-slept})]
    (apply max-key :count stats)))

(defn solve
  "find a guard by given strategy (key-fn) ... (1)
  find a minute (0-59) at which selected guard asleep most. ... (2)
  return a guard id multiplied by a minute that is (1) * (2)"
  [key-fn records]
  (let [sleepy-guard (apply max-key key-fn records)
        id           (first sleepy-guard)
        minute       (:minute (most-sleepy-minute sleepy-guard))]
    (* id minute)))

(defn part1
  "Strategy 1: Find the guard that has the most minutes asleep.
  What minute does that guard spend asleep the most?"
  [input]
  (let [mostly (fn [[_id intervals]]
                 (->> intervals
                      (map (fn [[begin end]] (- end begin)))
                      (apply +)))]
    (solve mostly (input->data input))))

(defn part2
  "Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?"
  [input]
  (let [frequently (fn [guard]
                     (:count (most-sleepy-minute guard)))]
    (solve frequently (input->data input))))

(comment

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
    (input->data input)
    #_#_(is (= 240 (part1 input)))
    (is (= 4455 (part2 input)))))