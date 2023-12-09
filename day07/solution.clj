(ns day07.solution
  (:require [clojure.string :as str]))

(def card-strengths ["A" "K" "Q" "J" "T" "9" "8" "7" "6" "5" "4" "3" "2"])
(def card-strengths2 ["A" "K" "Q" "T" "9" "8" "7" "6" "5" "4" "3" "2" "J"])

(defn parse [input]
  (map (fn [line]
         (let [[groups] (re-seq #"([\w\d]+)\ (\d+)" line)
               [_ hand bid] groups]
           (parse-long bid)
           {:hand (str/split hand #"")
            :bid (parse-long bid)}))
       (str/split-lines input)))


(defn optimize-hand [hand]
  (let [freq (frequencies hand)
        max-of-kind (reduce (fn [acc [k v]]
                              (if (not= "J" k) (max v acc) acc))
                            1
                            freq)
        best-hand-types (filter #(and (= max-of-kind (val %))) freq)
        best-hand-type (if (= 1 (count best-hand-types))
                         (key (first best-hand-types))
                         (nth card-strengths2 (reduce (fn [acc [k v]]
                                                        (min acc (.indexOf card-strengths2 k)))
                                                      (.indexOf card-strengths2 "J")
                                                      best-hand-types)))]
    ; replace joker with the optimal card
    (map #(if (= "J" %) best-hand-type %)
         hand)))

(defn compare-card-strengths [hand1 hand2 card-strengths]
  (loop [i 0 result nil]
    (if (and (< i (count hand1)) (not result))
       (recur
         (inc i)
         (let [card1 (nth hand1 i)
               card2 (nth hand2 i)
               strength-cmp (* -1 (compare (.indexOf card-strengths card1)
                                           (.indexOf card-strengths card2)))] ; reverse order. lower is better
           (if (not= strength-cmp 0) strength-cmp)))
      (if (not result) 0 result))))

(defn compare-hands [hand1 hand2 & [joker]]
  (let [hand1 (:hand hand1)
        hand2 (:hand hand2)
        freq1 (frequencies (if joker (optimize-hand hand1) hand1)) ; e.g.: {"A" 1, "B" 3, "C" 1}
        freq2 (frequencies (if joker (optimize-hand hand2) hand2))
        distinct-cmp (* -1 (compare (count freq1) (count freq2)))] ; reverse order; lower is better
    (if (= distinct-cmp 0)
      (let [hi-hand1 (reduce #(max %1 (second %2)) 0 freq1)
            hi-hand2 (reduce #(max %1 (second %2)) 0 freq2)
            hi-cmp (compare hi-hand1 hi-hand2)]      ; higher is better
        ; if hand type is equal: compare the card strengths of card 1 etc.
        (if (= hi-cmp 0)
          (compare-card-strengths hand1 hand2 (if joker card-strengths2 card-strengths))
          hi-cmp))
      distinct-cmp)))

(defn solve [input cmp-fn]
  (let [data (parse input)
        ranks (sort cmp-fn data)
        bids (map :bid ranks)]
    (reduce (fn [acc i]
              (+ acc
                 (* (inc i) (nth bids i))))
            0
            (range 0 (count ranks)))))

(defn solve1 [input]
  (solve input compare-hands))

(defn solve2 [input]
  (solve input (fn [hand1 hand2]
                 (compare-hands hand1 hand2 true))))

(defn main []
  (let [text (slurp "day07/sample-input.txt")
        input (slurp "day07/input.txt")
        sample-data (parse text)
        sample-hands (map :hand sample-data)]

    ; sample
    (println "optimize hands - sample2:  ")
    (println sample-hands)
    (println (map optimize-hand sample-hands))

    (println "solve sample1: " (solve1 text))
    (println "solve sample2: " (solve2 text))

    ;; solution
    (println "sum part1: " (solve1 input))
    (println "sum part2: " (solve2 input))
    ))
