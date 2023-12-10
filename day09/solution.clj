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

(defn get-last-col-vals [seq & acc]
  (let [diff-seq (calc-diff-seq seq)
        last-vals (if acc
                    (conj acc (last diff-seq))
                    (conj [(last seq)] (last diff-seq)))]
    (if (every? #(= (first seq) %) diff-seq)
      last-vals
      (recur diff-seq last-vals))))

(defn solve [input]
  (->> input
       (parse)
       (map get-last-col-vals)
       (map extrapolate)
       (reduce +)))

(defn main []
  (let [text (slurp "day09/sample-input.txt")
        input (slurp "day09/input.txt")]

    ; sample
    (println "last col values - sample 1: " (map get-last-col-vals (parse text)))
    (println "sum of extrapolated values - sample 1: " (solve text))

    ; solution
    (println "sum of extrapolated values - part1: " (solve input))
    ))

