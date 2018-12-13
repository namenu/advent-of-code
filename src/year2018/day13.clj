(ns year2018.day13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [util :refer [first-duplicate-key]]))

(def cart-comp #(let [[x1 y1] (:pos %1)
                      [x2 y2] (:pos %2)]
                  (if (= y1 y2)
                    (< x1 x2)
                    (< y1 y2))))

(defn input->state [input]
  (loop [lines (str/split-lines input)
         y     0
         track {}
         carts []]
    (if-let [s (first lines)]
      (let [cart?      {\> \- \< \- \^ \| \v \|}
            parse-path (fn [x ch]
                         [[x y] (or (cart? ch) ch)])
            track      (->> (map-indexed parse-path s)
                            (into track))
            carts      (->> (map vector (range) s)
                            (reduce (fn [acc [x ch]]
                                      (if (cart? ch)
                                        (conj acc {:pos [x y]
                                                   :dir ch
                                                   :opt (cycle [:left :straight :right])})
                                        acc))
                                    carts))]
        (recur (next lines) (inc y) track carts))
      {:track track
       :carts carts
       :tick  0})))

(def cross? #(= \+ %))
(def corner? #{\\ \/})

(def options {:left     {\> \^
                         \^ \<
                         \< \v
                         \v \>}
              :right    {\> \v
                         \v \<
                         \< \^
                         \^ \>}
              :straight identity})

(def corners {\/ {\> \^
                  \< \v
                  \^ \>
                  \v \<}
              \\ {\> \v
                  \< \^
                  \^ \<
                  \v \>}})

(defn move [pos dir]
  (let [moves {\< [-1 0]
               \> [1 0]
               \^ [0 -1]
               \v [0 1]}]
    (mapv + pos (moves dir))))

(defn update-cart [{:keys [pos dir opt] :as cart} track]
  (let [new-pos (move pos dir)
        path    (track new-pos)]
    (cond
      (cross? path)
      (let [dir2 ((options (first opt)) dir)]
        (-> cart
            (assoc :pos new-pos)
            (assoc :dir dir2)
            (update :opt next)))

      (corner? path)
      (let [dir2 ((corners path) dir)]
        (-> cart
            (assoc :pos new-pos)
            (assoc :dir dir2)))

      (= \space path)
      (prn "SPACED")

      :else
      (assoc cart :pos new-pos)
      )))

(defn collision [carts]
  (if-let [cart (first-duplicate-key :pos carts)]
    (:pos cart)))

(defn update-state [{:keys [track carts] :as state}]
  (loop [carts (vec (sort-by :pos carts))
         i     0]
    (if (< i (count carts))
      (let [cart      (get carts i)
            new-carts (assoc carts i (update-cart cart track))]
        (if-let [collision (collision new-carts)]
          (-> state
              (assoc :carts new-carts)
              (assoc :collision collision))
          (recur new-carts (inc i))))
      (-> state
          (assoc :carts carts)
          (update :tick inc)))))

(defn size [track]
  (let [[x y] (->> track (map first) (sort) last)]
    [(inc x) (inc y)]))

(defn print-state [{:keys [track carts]}]
  (let [track (into track (map (juxt :pos :dir) carts))
        [sx sy] (size track)]
    (doseq [y (range sy)]
      (doseq [x (range sx)]
        (print (track [x y])))
      (println))))

(def input (->> "day13.in" io/resource slurp))

(comment

  (def input "|\nv\n|\n|\n|\n^\n|\n")
  (def input "/->-\\        \n|   |  /----\\\n| /-+--+-\\  |\n| | |  | v  |\n\\-+-/  \\-+--/\n  \\------/   ")

  (loop [state (input->state input)]
    (prn (:tick state))
    (or (:collision state)
        (recur (update-state state))))


  #_(let [state* (iterate update-state state)
          state  (first (drop-while (complement collision) state*))]
      (let [out (with-out-str (print-state state))]
        (spit "out.0" out))
      (collision state))

  (->> (input->state input)
       (iterate update-state)
       #_(take-while (complement collision))
       (take 300)
       (map :carts)
       (map (fn [carts] (map #(dissoc % :opt) carts))))

  (nth (iterate update-state state) 0)
  (def state13
    (nth (iterate update-state state) 13))

  (:carts (update-state state13))
  )

;; tests
(require '[clojure.test :refer [deftest testing is run-tests]])
