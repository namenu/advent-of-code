(ns aoc.year2020.day08
  (:require [clojure.string :as str]
            [aoc.util :as aoc]))

(def sample "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")
(def input (aoc/input 2020 8))

(defn ->code [input]
  (let [parse (fn [s]
                (let [[inst val] (str/split s #" ")
                      val (Integer/parseInt val)]
                  [inst val]))]
    (->> (str/split-lines input)
         (map parse)
         (zipmap (range))
         (into {}))))

(defn init-state [code]
  {:ip      0
   :code    code
   :acc     0
   :history []})

(defn next-instr [{:keys [ip code]}]
  (code ip))

(defn index-of
  " do sequential search "
  [coll item]
  (loop [i 0 s coll]
    (when s
      (if (= (first s) item)
        i
        (recur (inc i) (next s))))))

(defn run [init-state]
  (loop [{:keys [ip code history] :as state} init-state]
    (let [[op val] (code ip)]
      ;(prn [op val] (dissoc state :code))
      (cond
        ; found an infinite-loop!
        (index-of history ip) [:infinity-loop (:acc state)]

        ; terminated gracefully
        (= ip (count code)) [:terminate (:acc state)]

        ; execute next instruction
        :else (let [state (-> state
                              (update :history conj ip)
                              (update :ip inc))]
                (case op
                  "nop" (recur state)
                  "jmp" (recur (assoc state :ip (+ ip val)))
                  "acc" (recur (update state :acc #(+ % val)))
                  ))))))

(defn part1 []
  (-> (->code input)
      (init-state)
      (run)))

(defn revise-at [code index]
  (let [[op val] (code index)]
    (case op
      "nop" (assoc code index ["jmp" val])
      "jmp" (assoc code index ["nop" val])
      nil)))

(defn part2 []
  (let [code (->code input)]
    (->> (keep #(revise-at code %) (range (count code)))
         (map (comp run init-state))
         (filter #(= (first %) :terminate)))))