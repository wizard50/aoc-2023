(ns day06.solution
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [lines (str/split-lines input)
        [_ time] (str/split (first lines) #":")
        [_ distance] (str/split (second lines) #":")
        time-records (map parse-long (re-seq #"\d+" time))
        dist-records (map parse-long (re-seq #"\d+" distance))]
    (for [i (range 0 (count dist-records)) ]
      [(nth time-records i) (nth dist-records i)])))

(defn parse2 [input]
  (let [lines (str/split-lines input)
        [_ time] (str/split (first lines) #":")
        [_ distance] (str/split (second lines) #":")
        t (str/join (re-seq #"\d+" time))
        d (str/join (re-seq #"\d+" distance))]
      [(parse-long t) (parse-long d)]))

(defn calc-distance [hold t]
  "d = (t - hold) * hold"
  (* (- t hold) hold))

(defn calc-ways-of-win [time distance]
  (let [results (for [hold (range 0 time)]
                  {:hold hold
                   :dist (calc-distance hold time)})]
    (filter #(> (:dist %) distance) results)))

(defn solve [input]
  (->> (parse input)
       (map #(calc-ways-of-win (first %) (second %)))
       (map count)
       (reduce *)))

(defn solve2 [input]
  (let [[t d] (parse2 input)]
    (count (calc-ways-of-win t d))))

(defn main []
  (let [text (slurp "day06/sample-input.txt")
        input (slurp "day06/input.txt")
        sample-data (parse text)
        [t1 d1] (first sample-data)]

    ; sample
    (println "sample1 data: " sample-data)
    (println "ways of win (first record) - sample1:" (calc-ways-of-win t1 d1))
    (println "product ways of win  - sample 1:" (solve text))

    ; solution
    (println "product ways of win - part1: " (solve input))
    (println "product ways of win - part2: " (solve2 input))))
