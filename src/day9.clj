(ns day9
  (:require [clojure.data.finger-tree :refer [double-list conjl]])
  (:import (java.util ArrayDeque)
           (clojure.data.finger_tree DoubleList)))

(defprotocol ICircle
  (ccw [_])
  (cw [_])
  (circle-push [_ v])
  (circle-pop [_])
  (circle-peek [_]))

(extend-type ArrayDeque
  ICircle
  (ccw [circle]
    (let [v (.removeLast circle)]
      (.addFirst circle v) circle))
  (cw [circle]
    (let [v (.removeFirst circle)]
      (.addLast circle v) circle))
  (circle-push [circle v]
    (.addLast circle v) circle)
  (circle-pop [circle]
    (.removeLast circle) circle)
  (circle-peek [circle]
    (.peekLast circle)))

(extend-type DoubleList
  ICircle
  (ccw [circle]
    (let [v (peek circle)]
      (-> circle (pop) (conjl v))))
  (cw [circle]
    (let [v (first circle)]
      (-> circle (rest) (conj v))))
  (circle-push [circle v]
    (conj circle v))
  (circle-pop [circle]
    (pop circle))
  (circle-peek [circle]
    (peek circle)))

(defn place [circle next-num]
  (if (zero? (mod next-num 23))
    (let [circle   (nth (iterate ccw circle) 7)
          score-at (circle-peek circle)]
      [(-> circle (circle-pop) (cw)) (+ score-at next-num)])
    [(-> circle (cw) (circle-push next-num)) 0]))

(defn play [circle num-players last-marble]
  (loop [circle    circle
         marble    1
         score-map {}]
    (if (> marble last-marble)
      (->> score-map (apply max-key val) (second))
      (let [[circle score] (place circle marble)
            player (mod marble num-players)]
        (recur circle (inc marble) (update score-map player (fnil + 0) score))))))


;; tests
(require '[clojure.test :refer [deftest testing is run-tests]])

(deftest test-day8
  (testing "Java interop"
    (is (= 386151 (time (play (ArrayDeque. [0]) 459 71790))))
    (is (= 3211264152 (time (play (ArrayDeque. [0]) 459 7179000))))
    "Elapsed time: 86.816502 msecs"
    "Elapsed time: 4782.187774 msecs")

  (testing "finger-tree impl"
    (is (= 386151 (time (play (double-list 0) 459 71790))))
    (is (= 3211264152 (time (play (double-list 0) 459 7179000))))
    "Elapsed time: 212.910395 msecs"
    "Elapsed time: 12702.448472 msecs"))

(run-tests)
