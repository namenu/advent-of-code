(ns year2018.day14)

(def max-l 293801)
(def max-l 9)
(def r (loop [i       0
              j       1
              recipes {0 3, 1 7}]
         (if (>= (count recipes) (+ max-l 10))
           recipes
           (let [vi (recipes i)
                 vj (recipes j)
                 x  (+ vi vj)

                 r  (if (< x 10)
                      recipes
                      (assoc recipes (count recipes) (quot x 10)))
                 r  (assoc r (count r) (rem x 10))

                 ii (mod (+ i vi 1) (count r))
                 jj (mod (+ j vj 1) (count r))
                 ]
             (recur ii jj r)
             )
           )))

(sort r)
(map r (range max-l (+ max-l 10)))


(def input [5 1 5 8 9])
(def input [9 2 5 1 0])
(def input [5 9 4 1 4])
(def input [2 9 3 8 0 1])

(time
  @(def r2
     (loop [i       0
            j       1
            recipes {0 3, 1 7}]
       (if (= input (map recipes (range (- (count recipes) (count input)) (count recipes))))
         (- (count recipes) (count input))
         (if (= input (map recipes (range (dec (- (count recipes) (count input))) (dec (count recipes)))))
           (- (count recipes) (count input) 1)
           (let [vi (recipes i)
                 vj (recipes j)
                 x  (+ vi vj)

                 r  (if (< x 10)
                      recipes
                      (assoc recipes (count recipes) (quot x 10)))
                 r  (assoc r (count r) (rem x 10))

                 ii (mod (+ i vi 1) (count r))
                 jj (mod (+ j vj 1) (count r))
                 ]
             (recur ii jj r))))
       )))
