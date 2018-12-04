(ns day3
  (:require [clojure.string :as str]))

(defn ->claim [s]
  (let [[_ id _ _ x y _ w h :as T] (str/split s #"[^\d]")
        [id x y w h] (mapv read-string [id x y w h])]
    {:id   id
     :rect [x y (+ x w) (+ y h)]}))


(defn overlap? [[ax1 ay1 ax2 ay2] [bx1 by1 bx2 by2]]
  (and (> ax2 bx1)
       (< ax1 bx2)
       (> ay2 by1)
       (< ay1 by2)))

(defn area [x1 y1 x2 y2]
  (* (- x2 x1) (- y2 y1)))

(defn part1 [input]
  (let [rects (map (comp :rect ->claim) input)
        [xs ys] (reduce (fn [[xs ys] [x1 y1 x2 y2]]
                          [(conj xs x1 x2) (conj ys y1 y2)])
                        [(sorted-set) (sorted-set)] rects)
        parts (for [[x1 x2] (partition 2 1 xs)
                    [y1 y2] (partition 2 1 ys)]
                [x1 y1 x2 y2])]
    (reduce + (map (fn [r]
                     (let [n-overlaps (->> rects
                                           (filter #(overlap? r %))
                                           count)]
                       (if (>= n-overlaps 2)
                         (apply area r)
                         0)))
                   parts))))


(defn part2 [input]
  (let [claims (map ->claim input)]
    (->> claims
         (filter (fn [{:keys [id rect]}]
                   (every? #(let [id2   (:id %)
                                  rect2 (:rect %)]
                              (or (= id id2)
                                  (not (overlap? rect rect2))))
                           claims)))
         first
         :id)))
