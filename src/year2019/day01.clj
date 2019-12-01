(ns year2019.day01
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))

(set! *unchecked-math* true)

(def input (let [lines (-> "year2019/day01.in" io/resource io/reader line-seq)]
             (map #(Integer/parseInt %) lines)))

(defn mass->fuel [m]
  (- (quot m 3) 2))

(defn mass->fuel2 [m]
  (let [mm (mass->fuel m)]
    (if (pos? mm)
      (+ mm (mass->fuel2 mm))
      0)))

(mass->fuel 1969)

(apply + (map mass->fuel input))

(apply + (map mass->fuel2 input))


;;
; numerical solution
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

(defn threes [m]
  (if (zero? (rem m 3))
    (inc (threes (quot m 3)))
    0))

(defn kn [m]
  (let [m' (+ m 3)
        n' (threes m')
        k  (quot m' (math/expt 3 n'))]
    [k (inc n')]))

; An = k*3^(n-1) - 3
(defn A [k n]
  (- (* k (math/expt 3 (dec n))) 3))

; Sn = k/2*3^(n-1) - 3n, S1 >= 0
(defn S [k n]
  (-> (math/expt 3 n)
      (- 1)
      (* k)
      (quot 2)
      (- (* 3 n))))

(defn f [m]
  (- (quot m 3) 2))

(defn F [m]
  (if (pos? m)
    (if (zero? (rem m 3))
      (let [[k n] (kn m)]
        (if (>= k 3)
          (+ (S k n) (F (f (A k 1))))
          (+ m (F (f m)))))
      (+ m (F (f m))))
    0))

(def mass->fuel2' #(F (f %)))

(comment
  "ensure both solutions give the same answers."
  (filter (comp false? second)
          (for [i (range 10000)]
            [i (= (mass->fuel2 i) (mass->fuel2' i))])))

(let [input input]
  [(time (apply + (map mass->fuel2 input)))
   (time (apply + (map mass->fuel2' input)))])

;; :(
(let [m (A 123456789 10000)]
  (time (mass->fuel2 m))
  (time (mass->fuel2' m)))
