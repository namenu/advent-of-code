;; --- Day 16: Flawed Frequency Transmission ---
(ns year2019.day16
  (:require [util :refer [input find-first bounding-box range-incl]]
            [graph :refer [bfs]]
            [year2019.intcode :refer :all]))

(defn pattern [phase]
  (->> (mapcat #(repeat phase %) [0 1 0 -1])
       (cycle)
       (rest)))

(def char->int
  (into {} (map vector "0123456789" (range 10))))

(defn abs [n]
  (Math/abs n))

(defn fft [signal phase]
  (let [v (->> (map #(* %1 %2) signal (pattern phase))
               (apply +))]
    (rem (abs v) 10)))

(defn next-signal [signal]
  (let [length (count signal)]
    (->> (range 1 (+ length 1))
         (map #(fft signal %)))))

(let [in     "11111111"
      in     "80871224585914546619083218645595"
      in     "19617804207202209144916044189917"
      in     "02935109699940807407585447034323"
      in     "03081770884921959731165446850517"
      in     (input 2019 16)
      signal (map char->int in)]
  ; pt.1
  #_(->> (iterate next-signal signal)
         (drop 100)
         (first)
         (take 8))

  ; pt.2

  ; find patterns from this
  #_(->> (map char->int "1111111111")
         (iterate next-signal)
         (map #(apply str %))
         (take 10))

  (let [offset  (Integer/parseInt (apply str (take 7 in)))
        length  (* 10000 (count signal))

        rlength (- length offset)
        rsignal (->> signal (reverse) (cycle) (take rlength))

        step-fn (fn [digits]
                  (reductions #(rem (+ %1 %2) 10) digits))]
    (->> (iterate step-fn rsignal)
         (drop 100)
         (first)
         (reverse)
         (take 8)))

  )
