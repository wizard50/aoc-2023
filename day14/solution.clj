(ns day14.solution
  (:require [clojure.string :as str]))

(defn parse [input]
  (str/split-lines input))

(defn transpose [lines]
  (map #(str/join %) (apply map list lines)))

(defn tilt-group [group]
  "move O's to the left side"
  (let [o (re-seq #"O" group)
        dot (re-seq #"\." group)]
    (str/join (concat o dot))))

(defn tilt [line]
  (let [groups (str/split line #"\#")                       ;NOTE: no groups for trailing hashes
        tilted (map tilt-group groups)
        result (str/join "#" tilted)
        missing-hash-count (- (count line) (count result))]
    (str/join (concat [result] (repeat missing-hash-count "#")))))

(defn calc-load [lines]
  (reduce (fn [acc i]
            (let [line (nth lines i)
                  o-count (count (re-seq #"O" line))
                  load (- (count lines) i)]
              (+ acc
                 (* load o-count))))
          0
          (range 0 (count lines))))

(defn solve [input]
  (->> (parse input)
       (transpose)
       (map tilt)
       (transpose)
       (calc-load)
       ))

(defn main []
  (let [text (slurp "day14/sample-input.txt")
        input (slurp "day14/input.txt")]

    ; sample
    (println "result - sample 1" (solve text))

    ; solution
    (println "result - part 1" (solve input))
    ))

