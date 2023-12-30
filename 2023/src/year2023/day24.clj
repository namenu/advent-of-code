(ns year2023.day24-2
  (:require [clojure.string :as s]))
;; inclusive
(def ^:dynamic *bounds* [7 27])

(defn in-range? [[px py]]
  (let [[min-bound max-bound] *bounds*]
    (and (<= min-bound px max-bound)
         (<= min-bound py max-bound))))

(defn point-at [[p v] t]
  (mapv #(+ %1 %2) p (mapv #(* % t) v)))

(defn intersection-tu
  ([[p1 :as h1] [p2 :as h2]]
   (intersection-tu p1 (point-at h1 1) p2 (point-at h2 1)))
  ([[x1 y1] [x2 y2] [x3 y3] [x4 y4]]
   (let [denom (- (* (- y4 y3) (- x2 x1)) (* (- x4 x3) (- y2 y1)))]
     (if (zero? denom)
       :parallel                                            ;; OR :coincide
       (let [t (/ (- (* (- x4 x3) (- y1 y3)) (* (- y4 y3) (- x1 x3)))
                  denom)
             u (/ (- (* (- x2 x1) (- y1 y3)) (* (- y2 y1) (- x1 x3)))
                  denom)]
         [t u])))))

(defn parse-input [s]
  (let [parse-line (fn [l]
                     (let [[ps vs] (s/split l #"@")]
                       [(->> (s/split ps #",") (mapv (comp bigint parse-long s/trim)))
                        (->> (s/split vs #",") (mapv (comp bigint parse-long s/trim)))]))]
    (->> (s/split-lines s)
         (map parse-line))))

(defn ordered-pairs [coll]
  (when-let [s (next coll)]
    (lazy-cat (for [y s] [(first coll) y])
              (ordered-pairs s))))

(def input "19, 13, 30 @ -2,  1, -2\n18, 19, 22 @ -1, -1, -2\n20, 25, 34 @ -2, -2, -4\n12, 31, 28 @ -1, -2, -1\n20, 19, 15 @  1, -5, -3")
(def input-large (slurp (clojure.java.io/resource "day24.in")))

(defn adjust-frame [[p v] sv]
  [p (mapv - v sv)])

(defn intersect-in-range? [h1 h2]
  (let [res (intersection-tu h1 h2)]
    (if (not= :parallel res)
      (let [[t u] res
            pa (take 2 (point-at h1 t))
            pb (take 2 (point-at h2 u))]
        (and (pos? t)
             (pos? u)
             #_#_(in-range? pa)
                     (in-range? pb))))))

(defn part1 [hails]
  (->> (ordered-pairs hails)
       (filter #(apply intersect-in-range? %))
       (count)))

(defn intersect-on-me? [hails sv]
  (let [;; frame adjusted hailstones
        [h1 h2 h3] (->> hails
                        (map #(adjust-frame % sv)))]
    (= true
       (intersect-in-range? h1 h2)
       (intersect-in-range? h2 h3)
       (intersect-in-range? h3 h1))))

;; xy 평면에서 교차함을 보장할 때 (skew 가 아닐 때)
(defn intersection [h1 h2]
  (let [[t u] (intersection-tu h1 h2)
        [_ _ z1 :as pa] (point-at h1 t)
        [_ _ z2] (point-at h2 u)]
    ;(prn t u z1 z2)
    (if (= z1 z2)
      pa)))

(defn intersect-on-me-3d? [hails sv]
  (let [;; frame adjusted hailstones
        [h1 h2 h3] (->> hails
                        (map #(adjust-frame % sv)))
        a (intersection h1 h2)
        b (intersection h2 h3)
        c (intersection h3 h1)]
    (when (and (some? a) (some? b) (some? c)
               (= a b c))
      (prn a)
      true)))

(comment

  (intersection-tu [[19.0 13.0 30.0] [-2.0 1.0 -2.0]]
                   [[18.0 19.0 22.0] [-1.0 -1.0 -2.0]])
  @(def hails (parse-input input))

  (binding [*bounds* [200000000000000 400000000000000]]
    (part1 (parse-input input-large)))

  (let [hails (take 3 (parse-input input))]
    (let [candidates (->> (for [svx (range -10 10)
                                svy (range -10 10)]
                            [svx svy 0])
                          (filter #(intersect-on-me? hails %)))]
      (->> (for [[svx svy] candidates
                 svz (range -10 10)]
             [svx svy svz])
           (filter #(intersect-on-me-3d? hails %)))))

  (time
    (def candidates
      (binding [*bounds* [200000000000000 400000000000000]]
        (let [hails (take 3 (parse-input input-large))]
          (->> (for [svx (range -500 501)
                     svy (range -500 501)]
                 [svx svy 0])
               (filter #(intersect-on-me? hails %))
               vec)))))

  (count candidates)

  (let [hails (take 3 (parse-input input-large))]
    (->> (for [[svx svy] candidates
               svz (range -100 101)]
           [svx svy svz])
         (pmap #(intersect-on-me-3d? hails %))
         (filter identity)
         ))

  #_(binding [*bounds* [200000000000000 400000000000000]]
      (let [candidates (let [hails (take 3 (parse-input input-large))]
                         (->> (for [svx (range -500 500)
                                    svy (range -500 500)]
                                [svx svy 0])
                              (filter #(intersect-on-me? hails %))))]
        (prn "#candidates: " (count candidates))
        (->> (for [[svx svy] candidates
                   svz (range -500 500)]
               [svx svy svz])
             (filter #(intersect-on-me-3d? hails %))
             )))
  )
