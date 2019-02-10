;; --- Day 9: Stream Processing ---
(ns year2017.day09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn stream->score [stream]
  (loop [[c & s] stream
         depth 0
         mode  :stream
         [score garbage] [0 0]]

    (if (nil? c)
      [score garbage]

      (case mode
        :stream (case c
                  \{
                  (recur s
                         (inc depth)
                         mode
                         [score garbage])

                  \}
                  (recur s
                         (dec depth)
                         mode
                         [(+ score depth) garbage])

                  \,
                  (recur s
                         depth
                         mode
                         [score garbage])

                  \<
                  (recur s
                         depth
                         :garbage
                         [score garbage]))

        :garbage (case c
                   \>
                   (recur s
                          depth
                          :stream
                          [score garbage])

                   \!
                   (recur s
                          depth
                          :ignore
                          [score garbage])

                   (recur s
                          depth
                          mode
                          [score (inc garbage)]))

        :ignore (case c
                  (recur s
                         depth
                         :garbage
                         [score garbage])
                  )
        )
      )))

(let [input (->> "year2017/day09.in" io/resource io/reader slurp str/trim)]
  (stream->score input))


;; tests
(require '[clojure.test :refer [deftest is run-tests]])

(deftest test
  (is (= 1 (first (stream->score "{}"))))
  (is (= 6 (first (stream->score "{{{}}}"))))
  (is (= 5 (first (stream->score "{{},{}}"))))
  (is (= 16 (first (stream->score "{{{},{},{{}}}}"))))
  (is (= 1 (first (stream->score "{<a>,<a>,<a>,<a>}"))))
  (is (= 9 (first (stream->score "{{<ab>},{<ab>},{<ab>},{<ab>}}"))))
  (is (= 9 (first (stream->score "{{<!!>},{<!!>},{<!!>},{<!!>}}"))))
  (is (= 3 (first (stream->score "{{<a!>},{<a!>},{<a!>},{<ab>}}"))))

  (is (= 0 (second (stream->score "<>"))))
  (is (= 17 (second (stream->score "<random characters>"))))
  (is (= 3 (second (stream->score "<<<<>"))))
  (is (= 2 (second (stream->score "<{!>}>"))))
  (is (= 0 (second (stream->score "<!!>"))))
  (is (= 0 (second (stream->score "<!!!>>"))))
  (is (= 10 (second (stream->score "<{o\"i!a,<{i<a>")))))

(run-tests)