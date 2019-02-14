(ns util)

(defn find-first [pred coll]
  (first (filter pred coll)))

(defn find-first-index [pred coll]
  (->> (map-indexed vector coll)
       (find-first (comp pred second))
       (first)))

(defn first-duplicate
  ([xs]
   (first-duplicate identity xs))
  ([key-fn xs]
   (let [result (reduce (fn [seen x]
                          (let [k (key-fn x)]
                            (if (seen k)
                              (reduced x)
                              (conj seen k))))
                        #{} xs)]
     (if (set? result)
       nil
       result))))

(defn find-cycle
  "Also can be implemented through a combination of first-duplicate & find-first."
  [coll]
  (loop [[c & rest] coll
         seen {c 0}
         nth  1]
    (let [c' (first rest)]
      (if-let [prev (seen c')]
        [prev nth]
        (recur rest (assoc seen c' nth) (inc nth))))))

(defn unique-one
  ([xs]
   (unique-one identity xs))
  ([key-fn [x y & xs]]
   (when xs
     (let [a (key-fn x), b (key-fn y)]
       (if (= a b)
         (find-first #(not= (key-fn %) a) xs)
         (let [z (first xs), c (key-fn z)]
           (if (= a c) y x)))))))

(defn rsort-by [keyfn coll]
  (sort-by keyfn #(compare %2 %1) coll))

(defn fixed-point [f x]
  (reduce #(if (= %1 %2) (reduced %1) %2)
          (iterate f x)))

(defn bounding-box [points]
  (let [cols    (apply map vector points)
        min-max (map #(apply (juxt min max) %) cols)]
    (apply map vector min-max)))

(defn range-incl
  ([end]
   (range (inc end)))
  ([start end]
   (range start (inc end)))
  ([start end step]
   (range start (inc end) step)))

(defn manhattan-dist [a b]
  (reduce + (map #(Math/abs ^Integer (- %1 %2)) a b)))
