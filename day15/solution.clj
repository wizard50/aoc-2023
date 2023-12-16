(ns day15.solution
  (:require [clojure.string :as str]))


(defn parse [input]
  (str/split input #","))

(defn hash-rfn [current-val ch]
  (-> current-val
      (+ (int ch))
      (* 17)
      (mod 256)))

(defn solve [input]
  (->> (parse input)
       (map #(reduce hash-rfn 0 %))
       (reduce +)))

(defn main []
  (let [text (slurp "day15/sample-input.txt")
        input (slurp "day15/input.txt")]

    ; sample
    (println "sample 'HASH' value: " (reduce hash-rfn 0 "HASH"))
    (println "result - sample 1: " (solve text))

    ; solution
    (println "result - part 1: " (solve input))

    ))

