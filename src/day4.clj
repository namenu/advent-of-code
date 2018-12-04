(ns day4
  (:require [clojure.string :as str]))

(defn parse-record [s]
  (let [[_ datetime msg] (re-find #"\[(.+)\] (.+)" s)
        record {:minute (->> (re-find #":(\d+)$" datetime) second Integer.)
                :type   (cond
                          (str/starts-with? msg "Guard") :shift
                          (str/starts-with? msg "falls") :asleep
                          (str/starts-with? msg "wakes") :awake)}]
    (if (= (:type record) :shift)
      (let [id (Integer. (re-find #"\d+" msg))]
        (assoc record :id id))
      record)))

(defn input->records [input]
  (let [records (map parse-record (sort input))
        by-id   (loop [[r & records] records
                       id  nil
                       out {}]
                  (if-let [{:keys [type minute]} r]
                    (if (= type :shift)
                      (recur records (:id r) out)
                      (recur records id (update out id (fnil conj []) minute)))
                    out))]
    (reduce-kv (fn [m k v] (assoc m k (partition 2 v))) {} by-id)))

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
