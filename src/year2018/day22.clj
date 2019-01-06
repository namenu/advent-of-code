(ns year2018.day22
  (:require [util :refer [range-incl manhattan-dist]]
            [clojure.data.priority-map :refer [priority-map]]))


;(def depth 510)
;(def target [10 10])
(def depth 3066)
(def target [13 726])
(def upper-bound (* 2 (dec (manhattan-dist [0 0] target))))


(def erosion
  (memoize
    (fn [[x y]]
      (let [modulo #(mod (+ % depth) 20183)]
        (cond
          (= [x y] [0 0])
          (modulo 0)

          (zero? y)
          (modulo (* x 16807))

          (zero? x)
          (modulo (* y 48271))

          (= [x y] target)
          (modulo 0)

          :else
          (modulo (* (erosion [(dec x) y]) (erosion [x (dec y)]))))))))

(defn cave [pos]
  (mod (erosion pos) 3))

(defn print-cave [target]
  (doseq [y (range-incl (get target 1))]
    (doseq [x (range-incl (get target 0))]
      (print (case (cave [x y])
               0 \.
               1 \=
               2 \|)))
    (println)))

(defn part1 [target]
  (reduce + (for [x (range-incl (get target 0))
                  y (range-incl (get target 1))]
              (cave [x y]))))


(defn addv [a b]
  (mapv + a b))

(defn suitable? [{:keys [pos equip]}]
  (let [pairs {0 #{:climb :torch}
               1 #{:climb :neither}
               2 #{:torch :neither}}
        type  (cave pos)]
    ((pairs type) equip)))

(defn moves [state reach]
  (let [m (reach state)]
    (->> [[-1 0] [1 0] [0 -1] [0 1]]
         (map #(update state :pos addv %))
         (filter #(let [[x y] (:pos %)]
                    (and (>= x 0) (>= y 0)
                         (suitable? %))))
         (filter #(let [v (or (reach %) upper-bound)]
                    (> v (inc m)))))))

(defn changes [{:keys [equip] :as state} reach]
  (let [m (reach state)]
    (->> (disj #{:climb :torch :neither} equip)
         (map #(assoc state :equip %))
         (filter suitable?)
         (filter #(let [v (or (reach %) upper-bound)]
                    (> v (+ m 7)))))))


(defn promising? [[from elapsed] reach]
  (let [estimate (+ elapsed
                    (manhattan-dist (:pos from) target)
                    (if (= (:equip from) :torch) 0 7))]
    (< estimate (or (reach target) upper-bound))))

(defn neighbors [[frontier elapsed] reach]
  (let [n1 (->> (moves frontier reach)
                (map #(vector % (inc elapsed))))
        n2 (->> (changes frontier reach)
                (map #(vector % (+ elapsed 7))))]
    (->> (into n1 n2))))


(defn part2 []
  (let [init  {:pos   [0 0]
               :equip :torch}
        reach (loop [reach {init 0}
                     queue (priority-map init 0)]
                (if-let [[frontier elapsed] (peek queue)]
                  (let [candidates (->> (neighbors [frontier elapsed] reach)
                                        (filter #(promising? % reach)))]
                    (recur (into reach candidates) (into (pop queue) candidates)))
                  reach))]
    (->> reach
         (filter #(= (:pos (first %)) target))
         (apply min-key second))))
