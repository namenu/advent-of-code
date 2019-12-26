;; --- Day 23: Category Six ---
(ns aoc.year2019.day23
  (:require [aoc.util :refer [input find-first fixed-point]]
            [aoc.year2019.intcode :refer :all])
  (:import (clojure.lang PersistentQueue)))


(def in (input 2019 23))

(defn make-computer [address]
  {:address address
   :nic     (-> (input->state in)
                (add-input address))
   :queue   PersistentQueue/EMPTY})

(defn recv [{:keys [queue nic] :as computer}]
  (if (empty? queue)
    (update computer :nic add-input -1)
    (assoc computer :nic (reduce #(add-input %1 %2) nic queue)
                    :queue PersistentQueue/EMPTY)))

(defn enqueue [computer x y]
  (update computer :queue conj x y))

(defn run-enough [{:keys [nic] :as computer}]
  (let [nic'   (run* nic)
        ; copy to buffer
        buffer (:output nic')

        ; flush output from nic
        nic'   (assoc nic' :output [])]
    [(assoc computer :nic nic') buffer]))

(defn send-to-nat [network x y]
  (assoc network :nat [x y]))

(defn send-network [network buffer]
  (->> (partition 3 buffer)
       (reduce (fn [network [addr x y]]
                 (if (= addr 255)
                   (send-to-nat network x y)
                   (update network addr enqueue x y)))
               network)))

(defn poll [network addr]
  (let [[computer' buffer] (-> (get network addr)
                               (recv)
                               (run-enough))]
    (-> network
        (assoc addr computer')
        (send-network buffer))))

(defn make-network [n]
  (let [computers (zipmap (range n) (map make-computer (range n)))]
    (assoc computers :nat nil)))

(let [network       (make-network 50)
      simulate-once (fn [network]
                      (reduce poll network (range 50)))]

  ; pt.1
  (->> (iterate simulate-once network)
       (find-first :nat)
       :nat)

  ; pt.2 - iterative
  #_(loop [network       network
         resume-packet nil]
    (let [network' (reduce poll network (range 50))]
      (if (= network network')                              ; idle?
        (let [[x y] (:nat network')]
          (if (= resume-packet [x y])
            y
            (recur (update network 0 enqueue x y) [x y])))
        (recur network' resume-packet))))

  ; pt.2 - sequential approach
  (let [simulate-until-idle (partial fixed-point simulate-once)
        resume-with-nat     (fn [network]
                              (let [[x y] (:nat network)]
                                (update network 0 enqueue x y)))]
    (->> (fixed-point (comp resume-with-nat simulate-until-idle) network)
         :nat)))
