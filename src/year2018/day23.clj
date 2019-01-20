(ns year2018.day23
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.data.priority-map :refer [priority-map]]
            [util :refer [manhattan-dist range-incl bounding-box]]))

(def input "pos=<0,0,0>, r=4\npos=<1,0,0>, r=1\npos=<4,0,0>, r=3\npos=<0,2,0>, r=1\npos=<0,5,0>, r=3\npos=<0,0,3>, r=1\npos=<1,1,1>, r=1\npos=<1,1,2>, r=1\npos=<1,3,1>, r=1\n")
(def input "pos=<10,12,12>, r=2\npos=<12,14,12>, r=2\npos=<16,12,12>, r=4\npos=<14,14,14>, r=6\npos=<50,50,50>, r=200\npos=<10,10,10>, r=5")
(def input (-> "day23.in" io/resource slurp))

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

(defn find-root [bots]
  (let [r (bit-shift-left 1 27)]
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

(time
  (part2 input))

(comment
  (require '[clojure.spec.test.alpha :as stest])
  (stest/instrument))