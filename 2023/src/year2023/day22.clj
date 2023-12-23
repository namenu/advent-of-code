(ns year2023.day22
  (:require [clojure.string :as str]
            [clojure.data :refer [diff]]))

(defn range-incl [a b]
  (range (min a b) (inc (max a b))))

;; "0,0,1~0,0,10"
(defn parse-brick
  [s]
  (let [[c1 c2] (str/split s #"~")
        parse-coord (fn [c]
                      (->> (str/split c #",")
                           (map parse-long)))
        [x1 y1 z1] (parse-coord c1)
        [x2 y2 z2] (parse-coord c2)]
    (for [x (range-incl x1 x2)
          y (range-incl y1 y2)
          z (range-incl z1 z2)]
      [x y z])))

;; return { bid => [coord] }
(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-brick)
       (zipmap (range))))

(def input "1,0,1~1,2,1\n0,0,2~2,0,2\n0,2,3~2,2,3\n0,0,4~0,2,4\n2,0,5~2,2,5\n0,1,6~2,1,6\n1,1,8~1,1,9\n")
(def input-large (slurp (clojure.java.io/resource "day22.in")))


(comment
  @(def bricks (parse-input input))
  (parse-brick "1,0,1~1,2,1")
  (parse-brick "0,0,1~0,0,10"))

(defrecord World [bricks occupied])

(defn get-brick-coords [world bid]
  (get (:bricks world) bid))

(defn create-world [bricks]
  (->World bricks (set (mapcat val bricks))))

(defn remove-brick [{:keys [bricks occupied]} bid]
  (->World (dissoc bricks bid)
           (apply disj occupied (get bricks bid))))

(defn add-brick [{:keys [bricks occupied]} bid coords]
  (->World (assoc bricks bid coords)
           (apply conj occupied coords)))

(defn try-fall [world bid]
  (let [b-coord    (map #(update % 2 dec) (get-brick-coords world bid))
        world'     (remove-brick world bid)
        grounded?  (every? #(pos? (get % 2)) b-coord)
        collapsed? (some (:occupied world') b-coord)]
    (if (and grounded? (not collapsed?))
      (add-brick world' bid b-coord)
      world)))

(comment
  (some #{1 2 3} [0 4])
  (let [bricks (parse-input input)]
    (-> (create-world bricks)
        (try-fall 2))))


(defn settle-one [world]
  (reduce
    (fn [world' bid]
      (try-fall world' bid))
    world
    (keys (:bricks world))))

(comment
  (let [bricks (parse-input input)
        world]
    (->World)
    (settle-one world)))

(defn settle-all [world]
  (loop [world world]
    ;(println "looped")
    (let [world' (settle-one world)]
      (if (= world world')
        world
        (recur world')))))



(defn can-disintegrate? [{:keys [bricks occupied] :as world} bid]
  (let [world' (remove-brick world bid)]
    (= world' (settle-one world'))))

(defn pfilter [pred coll]
  (filter identity (pmap pred coll)))

(defn chain-reactions [world bid]
  (let [[_ falls _] (diff (:bricks world)
                          (:bricks (settle-all (remove-brick world bid))))]
    (count falls)))


(comment
  ;; pt.1
  (time
    (let [;
          ;bricks  (parse-input input-large)
          bricks  (parse-input input)
          world   (create-world bricks)
          settled (settle-all world)]

      (->> (keys (:bricks settled))
           (pfilter #(can-disintegrate? settled %))
           (count))

      #_(chain-reactions settled 5)
      #_(count dbs)))

  ;; pt.2
  (time
    (let [;
          bricks  (parse-input input-large)
          ;bricks  (parse-input input)
          world   (create-world bricks)
          settled (settle-all world)]

      (->> (keys (:bricks settled))
           (pmap #(chain-reactions settled %))
           (apply +))))
  )