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

(defn tilt-north [lines]
  (->> lines
       (transpose)
       (map tilt)
       (transpose)))

(defn tilt-west [lines]
  (map tilt lines))

(defn tilt-south [lines]
  (->> lines
       (reverse)
       (tilt-north)
       (reverse)))

(defn tilt-east [lines]
  (->> lines
       (map #(str/reverse %))
       (tilt-west)
       (map #(str/reverse %))))

(defn one-cycle [lines]
  (-> lines
      (tilt-north)
      (tilt-west)
      (tilt-south)
      (tilt-east)))

(defn spin-cycle [lines n]
  "cache the pattern in a map and search for a period.
   the key is the string of the pattern, value the cycle number.
   the right period has to terminate on n.
   "
  (loop [c 1
         pattern lines
         seen (assoc {} (str/join lines) c)
         break? false]
    (if (and (<= c n) (not break?))
      (let [next-pattern (one-cycle pattern)
            p-key (str/join next-pattern)
            next-acc (assoc seen p-key c)
            last-c (get seen p-key)
            period (if last-c (- c last-c))
            next-break (and period (= 0 (mod (- n last-c) period)))]
        (recur
          (inc c)
          next-pattern
          next-acc
          next-break))
      pattern)))

(defn calc-load [lines]
  (reduce (fn [acc i]
            (let [line (nth lines i)
                  o-count (count (re-seq #"O" line))
                  load (- (count lines) i)]
              (+ acc (* load o-count))))
          0
          (range 0 (count lines))))

(defn solve [input]
  (->> (parse input)
       (transpose)
       (map tilt)
       (transpose)
       (calc-load)))

(defn solve2 [input]
  (-> (parse input)
      (spin-cycle 1000000000)
      (calc-load)))

(defn main []
  (let [text (slurp "day14/sample-input.txt")
        input (slurp "day14/input.txt")]

    ; sample
    (println "result - sample 1" (solve text))
    (println "result - sample 2" (solve2 text))

    ; solution
    (println "result - part 1" (solve input))
    (println "result - part 2" (solve2 input))
    ))