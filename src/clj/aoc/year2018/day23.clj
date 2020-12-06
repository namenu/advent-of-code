(ns aoc.year2018.day23
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map]]
            [aoc.util :refer [manhattan-dist range-incl bounding-box]]))


(s/def ::coord (s/tuple int? int? int?))
(s/def ::pos ::coord)
(s/def ::r int?)
(s/def ::bot (s/keys :req-un [::pos ::r]))

(defn input->bots [input]
  (let [parse-line (fn [s]
                     (let [[_ x y z r] (re-find #"pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)" s)]
                       {:pos (mapv #(Integer/parseInt %) [x y z])
                        :r   (Integer/parseInt r)}))]
    (map parse-line (str/split-lines input))))

(defn part1 [input]
  (let [bots    (input->bots input)
        largest (apply max-key :r bots)]
    (->> bots
         (filter #(<= (manhattan-dist (:pos largest) (:pos %)) (:r largest)))
         (count))))

;; Part-2
(s/def ::corner ::coord)
(s/def ::size int?)
(s/def ::node (s/keys :req-un [::corner ::size]))

(s/def ::interval (s/tuple int? int?))
(s/def ::cube (s/tuple ::interval ::interval ::interval))

(defn cube-dist [cube pos]
  (let [interval-dist (fn [[l r] x]
                        (cond
                          (< x l) (- l x)
                          (> x r) (- x r)
                          :else 0))]
    (apply + (map #(interval-dist %1 %2) cube pos))))

(defn overlap? [cube bot]
  (<= (cube-dist cube (:pos bot)) (:r bot)))

(defn node->cube [{:keys [corner size]}]
  (mapv #(vector % (dec (+ % size))) corner))

(defn subdivide [{:keys [corner size]}]
  (let [[x y z] corner
        sz (quot size 2)]
    (for [x [x (+ x sz)]
          y [y (+ y sz)]
          z [z (+ z sz)]]
      {:corner [x y z]
       :size   sz})))

(defn power-of-two [n]
  (letfn [(f [k]
            (if (>= k n)
              k
              (f (* k 2))))]
    (f 1)))

(defn find-root [bots]
  (let [bbox (bounding-box (map :pos bots))
        r    (->> (flatten bbox)
                  (map #(Math/abs %))
                  (apply max)
                  (power-of-two))]
    {:corner [(- r) (- r) (- r)]
     :size   (* r 2)}))

(defn part2 [input]
  (let [bots   (input->bots input)
        root   (find-root bots)
        weight (fn [node]
                 (let [cube (node->cube node)]
                   [(->> bots
                         (filter #(overlap? cube %))
                         (count)
                         (-))
                    (cube-dist cube [0 0 0])
                    (:size node)]))]
    (loop [queue (priority-map root (weight root))]
      (when-let [[node w] (peek queue)]
        (if (= (:size node) 1)
          [node w]
          (let [children (subdivide node)
                items    (map vector children (map weight children))]
            (recur (into (pop queue) items))))))))

(comment
  (def input (-> "year2018/day23.in" io/resource slurp))
  (time (part1 input))
  (time (part2 input))

  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument))
