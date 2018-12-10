(ns year2018.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [quil.core :as q]
            [quil.middleware :refer [fun-mode]]
            [util :refer [bounding-box]]))

(defn ->elf [s]
  (let [[_ pos vel] (re-find #"\<(.*)\>.*\<(.*)\>" (str/replace s #"\s" ""))
        parse-pair (fn [s] (mapv #(Integer/parseInt %) (str/split s #",")))]
    [(parse-pair pos) (parse-pair vel)]))

(defn update-pos [t elves]
  (map (fn [[[x y] [vx vy]]]
         [[(+ x (* t vx)) (+ y (* t vy))] [vx vy]]) elves))

(defn area2d [[[x1 y1] [x2 y2]]]
  (* (- x2 x1) (- y2 y1)))

(defn elves->area [elves]
  (-> (map first elves) (bounding-box) (area2d)))

(defn gather [elves]
  (loop [t     0
         elves elves
         area  (elves->area elves)]
    (let [new-elves (update-pos 1 elves)
          new-area  (elves->area new-elves)]
      (if (< new-area area)
        (recur (inc t) new-elves new-area)
        t))))

(def input (->> "day10.in" io/resource io/reader line-seq))
(def elves (map ->elf input))

; part2
(gather elves)

(def width 500)
(def max-t 10369)

(defn setup []
  (let [input     (->> "day10.in" io/resource io/reader line-seq)
        elves     (map ->elf input)
        bbox      (bounding-box (map first elves))
        max-width (- (first (second bbox)) (ffirst bbox))]
    {:elves elves
     :bbox  (bounding-box (map first elves))
     :scale (/ width max-width)
     :t     0.0}))

(defn update-state [{:keys [t elves scale] :as state}]
  (q/frame-rate 60)
  (if (>= t max-t)
    state
    (let [dt        (/ 1.0 scale)
          elves     (update-pos dt elves)
          bbox      (bounding-box (map first elves))
          max-width (- (first (second bbox)) (ffirst bbox))]
      (-> state
          (update :t + dt)
          (assoc :elves elves)
          (assoc :bbox bbox)
          (assoc :scale (/ width max-width))))))

(defn draw-state [{:keys [elves scale bbox]}]
  (q/background 0)
  (q/stroke 255)
  (q/stroke-weight 3)

  (q/translate 20 20)

  (let [translation (first bbox)]
    (doseq [[pos _] elves
            :let [[x y] (->> pos
                             (map #(- %2 %1) translation)
                             (map #(* scale %)))]]
      (q/point x y))))

(comment
  (def input (->> "day10.in" io/resource io/reader line-seq))
  (def elves (map ->elf input))

  (update-pos 1 elves)
  (let [scale 10000
        t     (/ scale)]
    (map (fn [[[x y] [vx vy]]]
           [[(+ x (* vx t)) (+ y (* vy t))] [vx vy]]) elves))

  (run-sketch)
  )

(defn ^:export run-sketch []
  (q/defsketch day10
    :size [540 540]
    :setup setup
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    :middleware [fun-mode]))


;; tests
(require '[clojure.test :refer [deftest testing is run-tests]])

(deftest test-day8
  (let [input ["position=< 9,  1> velocity=< 0,  2>"
               "position=< 7,  0> velocity=<-1,  0>"
               "position=< 3, -2> velocity=<-1,  1>"
               "position=< 6, 10> velocity=<-2, -1>"
               "position=< 2, -4> velocity=< 2,  2>"
               "position=<-6, 10> velocity=< 2, -2>"
               "position=< 1,  8> velocity=< 1, -1>"
               "position=< 1,  7> velocity=< 1,  0>"
               "position=<-3, 11> velocity=< 1, -2>"
               "position=< 7,  6> velocity=<-1, -1>"
               "position=<-2,  3> velocity=< 1,  0>"
               "position=<-4,  3> velocity=< 2,  0>"
               "position=<10, -3> velocity=<-1,  1>"
               "position=< 5, 11> velocity=< 1, -2>"
               "position=< 4,  7> velocity=< 0, -1>"
               "position=< 8, -2> velocity=< 0,  1>"
               "position=<15,  0> velocity=<-2,  0>"
               "position=< 1,  6> velocity=< 1,  0>"
               "position=< 8,  9> velocity=< 0, -1>"
               "position=< 3,  3> velocity=<-1,  1>"
               "position=< 0,  5> velocity=< 0, -1>"
               "position=<-2,  2> velocity=< 2,  0>"
               "position=< 5, -2> velocity=< 1,  2>"
               "position=< 1,  4> velocity=< 2,  1>"
               "position=<-2,  7> velocity=< 2, -2>"
               "position=< 3,  6> velocity=<-1, -1>"
               "position=< 5,  0> velocity=< 1,  0>"
               "position=<-6,  0> velocity=< 2,  0>"
               "position=< 5,  9> velocity=< 1, -2>"
               "position=<14,  7> velocity=<-2,  0>"
               "position=<-3,  6> velocity=< 2, -1>"]]
    (is (= 3 (time (part2 input)))))
  )

;(run-tests)
