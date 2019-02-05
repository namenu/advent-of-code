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

(def input (->> "year2018/day10.in" io/resource io/reader line-seq))
(def elves (map ->elf input))

; part2 answer
(def end-t (gather elves))


;; rendition

(defn lerp2
  "Simple easing function with double lerp."
  [end a b]
  (fn [start]
    (-> start
        (q/lerp end a)
        (q/lerp end b))))

(def width 500)
(def timing-fn (lerp2 end-t 0.05 0.005))
(def epsilon 1e-4)

(defn setup []
  (q/frame-rate 60)
  {:elves elves
   :t     0.0})

(defn update-state [{:keys [t elves] :as state}]
  (let [t' (timing-fn t)
        dt (- t' t)]
    (if (< dt epsilon)
      state
      (-> state
          (assoc :elves (update-pos dt elves))
          (assoc :t t')))))

(defn draw-state [{:keys [elves]}]
  (q/background 0)
  (q/stroke 255)
  (q/stroke-weight 3)

  ; padding
  (q/translate 20 20)

  (let [bbox        (bounding-box (map first elves))
        max-width   (- (first (second bbox)) (ffirst bbox))
        scale       (/ width max-width)
        translation (first bbox)]
    (doseq [[pos _] elves
            :let [[x y] (->> pos
                             (map #(- %2 %1) translation)
                             (map #(* scale %)))]]
      (q/point x y))))

(comment
  (def input (->> "year2018/day10.in" io/resource io/reader line-seq))
  (def elves (map ->elf input))

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

