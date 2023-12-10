(ns day09.solution
  (:require [clojure.string :as str]))

(defn parse [input]
  (map (fn [line]
         (map parse-long (re-seq #"-?\d+" line)))
       (str/split-lines input)))

(defn calc-diff-seq [seq]
  (for [i (range 1 (count seq))]
    (- (nth seq i) (nth seq (dec i)))))

(defn extrapolate [last-col-vals]
  (reduce + last-col-vals))

(defn calc-diff-rows [seq & acc]
  (let [diff-seq (calc-diff-seq seq)
        last-vals (if acc
                    (conj acc diff-seq)
                    (conj [seq diff-seq]))]
    (if (every? zero? diff-seq)
      last-vals
      (recur diff-seq last-vals))))

(defn solve [lines]
  (->> lines
       (map calc-diff-rows)
       (map #(map last %))
       (map extrapolate)
       (reduce +)))

(defn solve1 [input]
  (solve (parse input)))

(defn solve2 [input]
  (solve (map reverse (parse input))))

(defn main []
  (let [text (slurp "day09/sample-input.txt")
        input (slurp "day09/input.txt")]

    ; sample
    (println "sum of extrapolated values - sample 1: " (solve1 text))
    (println "sum of extrapolated values - sample 2: " (solve2 text))

    ; solution
    (println "sum of extrapolated values - part1: " (solve1 input))
    (println "sum of extrapolated values - part2: " (solve2 input))
    ))

