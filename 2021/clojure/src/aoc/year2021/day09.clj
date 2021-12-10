(ns aoc.year2021.day09
  (:require [clojure.string :as str]))

(def input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")

(def input (slurp "resources/day09.in"))

(defn check [line]
  (let [opening  #{\{ \( \[ \<}
        matching {\) \(, \] \[, \} \{, \> \<}]
    (reduce (fn [stk b]
              (if (opening b)
                (conj stk b)
                (if (= (peek stk) (matching b))
                  (pop stk)
                  (reduced b))))
            []
            line)))

;; pt1
(let [score {\) 3, \] 57, \} 1197, \> 25137}]
  (->> (str/split-lines input)
       (map check)
       (map #(score % 0))
       (apply +)))

(defn completion-score [openings]
  (let [score {\( 1, \[ 2, \{ 3, \< 4}]
    (loop [stk   openings
           total 0]
      (if (seq stk)
        (recur (pop stk)
               (+ (* total 5) (score (peek stk))))
        total))))

;; pt2
(let [scores (->> (str/split-lines input)
                  (map check)
                  (remove char?)
                  (map completion-score))]
  (nth (sort scores) (quot (count scores) 2)))