(ns aoc.year2022.day11
  (:require [aoc.math :refer [divisible? lcm]]))

(def monkeys [{:items     [79 98]
               :operation #(* % 19)
               :test      #(if (divisible? % 23) 2 3)
               :inspected 0}
              {:items     [54 65 75 74]
               :operation #(+ % 6)
               :test      #(if (divisible? % 19) 2 0)
               :inspected 0}
              {:items     [79 60 97]
               :operation #(* % %)
               :test      #(if (divisible? % 13) 1 3)
               :inspected 0}
              {:items     [74]
               :operation #(+ % 3)
               :test      #(if (divisible? % 17) 0 1)
               :inspected 0}])

(def ^:dynamic *reliever* #(quot % 3))
(def ^:dynamic *round* 20)

(defn turn [{:keys [items operation test]}]
  (->> (map operation items)
       (map *reliever*)
       (group-by test)))

(defn round [monkeys]
  (reduce
    (fn [monkeys monkey-no]
      (let [monkey         (get monkeys monkey-no)
            currently-have (count (:items monkey))
            inspect&flush  (fn [monkey]
                             (-> monkey
                                 (update :inspected + currently-have)
                                 (assoc :items [])))]
        (reduce (fn [state [to items]]
                  (update-in state [to :items] into items))
                (update monkeys monkey-no inspect&flush)
                (turn monkey))))
    monkeys
    (range 0 (count monkeys))))

(defn inspect [monkeys]
  (->> monkeys
       (map-indexed (fn [no monkey] [no (:inspected monkey)]))
       (into {})))

(defn monkey-business [monkeys]
  (let [inspections (->> (nth (iterate round monkeys) *round*)
                         (map :inspected))
        [m1 m2] (sort (comp - compare) inspections)]
    (* m1 m2)))



(comment
  ;; pt1
  (monkey-business monkeys)
  ;; => 54752


  ;; pt2
  (let [l (lcm 23 19 13 17)
        l (lcm 19 3 13 7 5 11 17 2)]
    (binding [*reliever* #(mod % l)
              *round* 10000]
      (monkey-business monkeys)))


  (def monkeys [{:items     [85 77 77]
                 :operation #(* % 7)
                 :test      #(if (divisible? % 19) 6 7)
                 :inspected 0}
                {:items     [80 99]
                 :operation #(* % 11)
                 :test      #(if (divisible? % 3) 3 5)
                 :inspected 0}
                {:items     [74, 60, 74, 63, 86, 92, 80]
                 :operation #(+ % 8)
                 :test      #(if (divisible? % 13) 0 6)
                 :inspected 0}
                {:items     [71, 58, 93, 65, 80, 68, 54, 71]
                 :operation #(+ % 7)
                 :test      #(if (divisible? % 7) 2 4)
                 :inspected 0}

                {:items     [97, 56, 79, 65, 58]
                 :operation #(+ % 5)
                 :test      #(if (divisible? % 5) 2 0)
                 :inspected 0}
                {:items     [77]
                 :operation #(+ % 4)
                 :test      #(if (divisible? % 11) 4 3)
                 :inspected 0}
                {:items     [99, 90, 84, 50]
                 :operation #(* % %)
                 :test      #(if (divisible? % 17) 7 1)
                 :inspected 0}
                {:items     [50, 66, 61, 92, 64, 78]
                 :operation #(+ % 3)
                 :test      #(if (divisible? % 2) 5 1)
                 :inspected 0}]))