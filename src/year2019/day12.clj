;; --- Day 12: The N-Body Problem ---
(ns year2019.day12
  (:require [util :refer [input find-first-index]]))

(defn update-moon [moon moons]
  (let [cmp (fn [a b]
              (cond (< a b) 1
                    (> a b) -1
                    :else 0))
        vel (->> (for [other moons
                       :when (not= other moon)]
                   (mapv cmp (:pos moon) (:pos other)))
                 (reduce #(mapv + %1 %2) (:vel moon)))]
    {:pos (mapv + (:pos moon) vel)
     :vel vel}))

(defn update-system [moons]
  (loop [[m & next] moons
         new-moons []]
    (if m
      (recur next (conj new-moons (update-moon m moons)))
      new-moons)))

(defn abs [x]
  (Math/abs x))

(defn moon-energy [moon]
  (* (apply + (map abs (:pos moon)))
     (apply + (map abs (:vel moon)))))

(defn system-energy [moons]
  (apply + (map moon-energy moons)))

(defn wave-length
  [coll]
  (let [start (first coll)]
    (+ 2 (find-first-index #(= start %) (next coll)))))

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

#_(input 2019 12)
(let [moons [{:pos [-1 0 2] :vel [0 0 0]}
             {:pos [2 -10 -7] :vel [0 0 0]}
             {:pos [4 -8 8] :vel [0 0 0]}
             {:pos [3 5 -1] :vel [0 0 0]}]
      moons [{:pos [-8 -10 0] :vel [0 0 0]}
                 {:pos [5 5 10] :vel [0 0 0]}
                 {:pos [2 -7 3] :vel [0 0 0]}
                 {:pos [9 -8 -3] :vel [0 0 0]}]
      #_#_moons [{:pos [19 -10 7] :vel [0 0 0]}
                 {:pos [1 2 -3] :vel [0 0 0]}
                 {:pos [14 -4 1] :vel [0 0 0]}
                 {:pos [8 7 -6] :vel [0 0 0]}]
      ]
  ; pt.1
  #_(let [step 1000]
      (-> (iterate update-system moons)
          (nth step)
          (system-energy)))

  ; pt.2
  (let [filter-axis (fn [axis]
                      (fn [moons]
                        (mapv #(get (:pos %) axis) moons)))
        lengths     (for [axis [0 1 2]]
                      (->> (iterate update-system moons)
                           (map (filter-axis axis))
                           (wave-length)))]
    (apply lcm lengths)))
