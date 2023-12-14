(ns day13.solution
  (:require [clojure.string :as str]))


(defn parse [input]
  (->> (str/split input #"\n\n")
       (map #(str/split-lines %))))

(defn search-v-reflection [lines]
  (for [x (range 1 (count (first lines)))
        :when (every? #(= (first %) (second %))
                       (map #(let [line (nth lines %)
                                   len (min x (- (count line) x))
                                   start (if (<= x len)
                                           0
                                           (- (count line) (* len 2)))
                                   col1 (subs line start (+ start len))
                                   col2 (subs line (+ start len) (+ start (* 2 len)))]
                               [col1 (str/reverse col2)])
                            (range 0 (count lines))))]
    x))

(defn search-h-reflection [lines]
  (for [y (range 1 (count lines))
        :let [len (min y (- (count lines) y))
              start (if (<= y len)
                      0
                      (- (count lines) (* len 2)))
              rows1 (str/join (reverse (take len (nthnext lines start))))
              rows2 (str/join (take len (nthnext lines (+ start len))))]
        :when (= rows1 rows2)]
    y))

(defn solve [input]
  (let [pattern-list (parse input)]
    (reduce (fn [acc pattern]
              (let [h (first (search-h-reflection pattern))
                    v (first (search-v-reflection pattern))]
                (+ acc (if v v 0)
                       (if h (* h 100) 0))))
            0
            pattern-list)))

(defn main []
  (let [text (slurp "day13/sample-input.txt")
        input (slurp "day13/input.txt")]

    ; sample
    (println "result - sample 1: " (solve text))

    ; solution
    (println "result - part 1: " (solve input))
    ))

