;; --- Day 22: Slam Shuffle ---
(ns aoc.year2019.day22
  (:require [aoc.util :refer [input-lines find-first]]
            [clojure.math.numeric-tower :refer [expt]]
            [clojure.string :as str]))

(def ^:dynamic D 119315717514047)                           ; Deck size
(def ^:dynamic R 101741582076661)                           ; Repetition count

; 세 연산은 ax+b (mod D) 형태의 변환의 합성으로 표현할 수 있다. (LCG)
; 스택 뒤집기 ax + b -> -ax + -b-1
; 컷 ax + b, n -> ax + b-n
; 건너뛰기 ax + b, n -> anx + nb
; 시작 상태는 0~D-1까지 정렬된 상태라고 했으므로, a=1, b=0으로 초항이 결정된다.

(defn deal-stack-of [[a b]]
  [(- a), (- (- b) 1)])

(defn cut-deck-of [n [a b]]
  [a, (mod (- b n) D)])

(defn deal-inc-of [n [a b]]
  [(mod (* a n) D), (mod (* b n) D)])

(defn solve [[a b] x]
  (mod (+ (* a x) b) D))

(defn parse-line [line stack-fn cut-fn inc-fn]
  (let [[a b _ d] (str/split line #" ")]
    (if (= a "cut")
      (partial cut-fn (Long/parseLong b))
      (if (= b "with")
        (partial inc-fn (Long/parseLong d))
        stack-fn))))

; pt.1
(binding [D 10007]
  (let [shuffles (map #(parse-line % deal-stack-of cut-deck-of deal-inc-of) (input-lines 2019 22))
        F        (apply comp (reverse shuffles))]
    (solve (F [1 0]) 2019)))


; pt.2

; 전체 셔플셋을 F라 했을 때, F^-1를 R번 한 결과를 찾아야 한다.
; 만약 F가 f.g.h 라면, F^-1는 h^-1.g^-1.h^-1 형태가 된다.
; 스택 뒤집기 f(x) 역은 f(x)
; 컷 g(x)의 역은 g(-n, x)
; 건너뛰기 h(x)의 역은 h(modInv(n))
; modInv는 내장함수를 이용하였음.
;
; (F^-1)^R 에서 R은 매우 큰 값이 나오는데, F^-1도 1차 다항식의 모노이드 연산임을 고려할 때
; (ax+b)^n = a^nx + b(a^n-1)(a-1)^1
; 형태로 단순화 할 수 있으며, 빠른 지수승 계산에는 modPow내장함수를 이용하였음.


(defn mod-inv [x]
  (.modInverse (biginteger x) (biginteger D)))

(defn mod-exp [x n]
  (.modPow (biginteger x) (biginteger n) (biginteger D)))

(def inv-deal-stack-of
  deal-stack-of)

(defn inv-cut-deck-of [n [a b]]
  (cut-deck-of (- n) [a b]))

(defn inv-deal-inc-of [n [a b]]
  (deal-inc-of (mod-inv n) [a b]))

(defn repeat-of [n [a b]]
  [(mod-exp a n), (mod (* b (* (- (mod-exp a n) 1) (mod-inv (- a 1)))) D)])

(let [shuffles (map #(parse-line % inv-deal-stack-of inv-cut-deck-of inv-deal-inc-of) (input-lines 2019 22))
      inv-F    (apply comp shuffles)]
  (solve (repeat-of R (inv-F [1 0])) 2020))
