;; --- Day 1: The Tyranny of the Rocket Equation ---
(ns year2019.day01
  (:require [util :refer [input-lines]]
            [clojure.math.numeric-tower :as math]))

(set! *unchecked-math* true)

(def input (map #(Integer/parseInt %) (input-lines 2019 1)))

(defn mass->fuel [m]
  (- (quot m 3) 2))

(defn mass->fuel* [m]
  (->> (iterate mass->fuel m)
       (take-while pos?)
       (drop 1)
       (apply +)))

; pt.1
(apply + (map mass->fuel input))

; pt.2
(apply + (map mass->fuel* input))


;;
; numerical-analytical solution
;
; m은 등비수열 A(k,n)으로 표현할 수 있음
; 0보다 큰 m 에 대해,
; 1. m이 3의 배수이면
;  - S(k,n)을 결과에 더하고 (k > 3, ∵ S1 >= 0)
;  - m := f(A(k,1))
; 2. m이 3의 배수가 아니라면
;  - f(m)을 결과에 더하고
;  - m := f(m)
; 이 떄, S(k,n)은 등비수열의 합

(defn kn [m]
  (loop [m (+ m 3) cnt 1]
    (let [r (rem m 3)]
      (if (zero? r)
        (recur (quot m 3) (inc cnt))
        [m cnt]))))

; An = k*3^(n-1) - 3
; (for test)
(defn A [k n]
  (- (* k (math/expt 3 (dec n))) 3))

; Sn = k/2*(3^n-1) - 3n, S1 >= 0
;    = (3*An+9-k-6n) / 2
(defn S [m k n]
  (-> (* m 3)
      (+ 9)
      (- k)
      (- (* 6 n))
      (quot 2)))

(def f mass->fuel)

(defn F [m]
  (let [m (f m)]
    (if (pos? m)
      (if (zero? (rem m 3))
        (let [[k n] (kn m)]
          (if (>= k 3)
            (+ (S m k n) (F (- k 3)))
            (+ m (F m))))
        (+ m (F m)))
      0)))

(def mass->fuel*' F)

(comment
  "ensure both solutions give the same answers."
  (filter (comp false? second)
          (for [i (range 10000)]
            [i (= (mass->fuel* i) (mass->fuel*' i))]))

  (time (count (for [i (range 1000000)] (mass->fuel* i))))
  (time (count (for [i (range 1000000)] (mass->fuel*' i))))

  (let [input input]
    [(time (apply + (map mass->fuel* input)))
     (time (apply + (map mass->fuel*' input)))])

  ; 4931831
  (apply + (map mass->fuel*' input))

  ;; the analytical solution is much slower.. :(
  (let [m (A 123456789 1000)]
    (time (mass->fuel* m))
    (time (mass->fuel*' m)))
  )
