(ns aoc.year2020.day08
  (:require [clojure.string :as str]
            [aoc.util :as aoc]))

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
  {:ip   0
   :code code
   :acc  0})

(defmulti exec (fn [_state inst] (first inst)))

(defmethod exec "nop" [state _]
  (update state :ip inc))

(defmethod exec "jmp" [state [_ val]]
  (update state :ip + val))

(defmethod exec "acc" [state [_ val]]
  (-> (update state :acc + val)
      (update :ip inc)))


(defn run [init-state]
  (loop [{:keys [ip code] :as state} init-state
         history #{}]
    (cond
      ; found an infinite-loop!
      (history ip) [:infinity-loop (:acc state)]

      ; terminated gracefully
      (= ip (count code)) [:terminate (:acc state)]

      ; execute next instruction
      :else (recur (exec state (code ip))
                   (conj history ip)))))

(defn part1 [input]
  (-> (->code input)
      (init-state)
      (run)))

(defn revise-at
  "index에 해당하는 메모리에 변화를 줍니다.
  변화를 줄 수 없으면(실패시) nil을 반환"
  [code index]
  (let [[op val] (code index)]
    (case op
      "nop" (assoc code index ["jmp" val])
      "jmp" (assoc code index ["nop" val])
      nil)))

(defn part2 [input]
  (let [code (->code input)]
    (->> (keep #(revise-at code %) (range (count code)))
         (map (comp run init-state))
         (aoc/find-first #(= (first %) :terminate)))))

(comment
  (def sample "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")
  (def input (aoc/input 2020 8))

  (part1 input)
  (time
    (part2 input))
  )