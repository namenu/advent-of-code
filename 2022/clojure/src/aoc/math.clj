(ns aoc.math)

(defn divisible? [x d]
  (zero? (mod x d)))

(defn gcd [a b]
  (if (zero? b)
    a
    (if (> a b)
      (gcd b (rem a b))
      (gcd b a))))

(defn lcm
  ([a b]
   (quot (* a b) (gcd a b)))
  ([a b & c]
   (reduce lcm a (cons b c))))

