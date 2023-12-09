(ns day07.solution
  (:require [clojure.string :as str]))

(def card-strengths ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])

(defn parse [input]
  (map (fn [line]
         (let [[groups] (re-seq #"([\w\d]+)\ (\d+)" line)
               [_ hand bid] groups]
           (parse-long bid)
           {:hand (str/split hand #"")
            :bid (parse-long bid)}))
       (str/split-lines input)))

(defn compare-card-strengths [hand1 hand2]
  (loop [i 0 result nil]
    (if (and (< i (count hand1)) (not result))
       (recur
         (inc i)
         (let [card1 (get hand1 i)
               card2 (get hand2 i)
               strength-cmp (* -1 (compare (.indexOf card-strengths card1)
                                           (.indexOf card-strengths card2)))] ; reverse order. lower is better
           (if (not= strength-cmp 0) strength-cmp)))
      result)))

(defn compare-hands [hand1 hand2]
  (let [freq1 (frequencies hand1)                           ; e.g.: {"A" 1, "B" 3, "C" 1}
        freq2 (frequencies hand2)
        distinct-cmp (* -1 (compare (count freq1) (count freq2)))] ; reverse order; lower is better
    (if (= distinct-cmp 0)
      (let [hi-hand1 (reduce #(max %1 (second %2)) 0 freq1)
            hi-hand2 (reduce #(max %1 (second %2)) 0 freq2)
            hi-cmp (compare hi-hand1 hi-hand2)]      ; higher is better
        ; if hand type is equal: compare the card strengths of card 1 etc.
        (if (= hi-cmp 0)
          (compare-card-strengths hand1 hand2)
          hi-cmp))
      distinct-cmp)))

(defn solve [input]
  (let [data (parse input)
        hands (map :hand data)
        ranks (sort compare-hands hands)
        bids (map #(nth (map :bid data)
                        (.indexOf hands %)) ranks)]
    (reduce (fn [acc i]
              (+ acc (* (inc i) (nth bids i))))
            0
            (range 0 (count ranks)))))

(defn main []
  (let [text (slurp "day07/sample-input.txt")
        input (slurp "day07/input.txt")
        sample-data (parse text)
        sample-hands (map :hand sample-data)]

    ; sample
    (println "compare hands - sample 1: " (nth sample-hands 1) (nth sample-hands 2)
             (compare-hands (nth sample-hands 1) (nth sample-hands 2)))

    (println "compare card strengths - sample 1: " (nth sample-hands 2) (nth sample-hands 3)
             (compare-card-strengths (nth sample-hands 2) (nth sample-hands 3)))

    (println "hand ranking - sample 1" (sort compare-hands sample-hands))
    (println "solve sample1: " (solve text))

    ;; solution
    (println "sum part1: " (solve input))
    ))

