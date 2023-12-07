(ns day04.solution
  (:require [clojure.string :as str]))

(defn parse-cards [lines]
  (reduce (fn [cards line]
            (let [[card-num rest] (str/split line #":")
                  card-id (->> (str/split card-num #" ")
                               (filter #(not (str/blank? %)))
                               (second)
                               (Integer/parseInt))
                  [numbers1 numbers2] (str/split rest #" \| ")
                  winning-numbers (->> (str/split numbers1 #" ")
                                        (map #(str/trim %))
                                        (filter #(not (str/blank? %)))
                                        (map #(Integer/parseInt %)))
                  my-numbers (->> (str/split numbers2 #" ")
                                  (map #(str/trim %))
                                  (filter #(not (str/blank? %)))
                                  (map #(Integer/parseInt %)))]
              (conj cards {:card-id card-id
                           :winning-numbers winning-numbers
                           :my-numbers my-numbers}))) [] lines))

(defn get-winning-numbers [card]
  (reduce (fn [res w]
            (if (some #(= w %) (:my-numbers card))
              (conj res w)
              res))
          [] (:winning-numbers card)))

(defn calc-points [owned-winning-numbers]
  (->> (filter #(not (empty? %)) owned-winning-numbers)
       (map #(Math/pow 2 (dec (count %))))))

(defn increment-copy-instances [res card-id copies]
  "increment count for card i .. card i + copies"
  (reduce (fn [res j]
            (let [old-count (get res j 0)]
              (assoc res j (inc old-count))))
          res
          (range (inc card-id) (+ (inc card-id) copies))))

(defn calc-copies [cards]
  (reduce (fn [res i]
            (let [card (nth cards i)
                  card-id (inc i)
                  winning-numbers (get-winning-numbers card)
                  copies (get res card-id 0)]

              (if (> (count winning-numbers) 0)
                ; increment count for next cards
                ; do this for the origin card and for all copies
                (reduce (fn [acc n]
                          (increment-copy-instances acc card-id (count winning-numbers)))
                        res (range 0 (inc copies)))
                res)))
          {} (range 0 (count cards))))

(defn calc-copy-sum [res]
  (reduce (fn [acc a]
            (+ acc (val a)))
          0 res))

(defn solve [input]
  (->> input
      (str/split-lines)
      (parse-cards)
      (map get-winning-numbers)
      (calc-points)
      (reduce +)))

(defn solve2 [input]
  (let [lines (str/split-lines input)
        cards (parse-cards lines)
        copies-res (calc-copies cards)
        copy-count (calc-copy-sum copies-res)]
    (+ copy-count (count cards))))

(defn main []
  (let [text (slurp "day04/sample-input.txt")
        input (slurp "day04/input.txt")
        numbers-sample1 (->> (str/split-lines text)
                             (parse-cards)
                             (map get-winning-numbers))
        points-sample1  (calc-points numbers-sample1)
        copies-sample2 (->> text
                            (str/split-lines)
                            (parse-cards)
                            (calc-copies))]

    ; sample
    (println "winning numbers - sample1: " numbers-sample1)
    (println "points - sample1: " points-sample1)
    (println "sum sample1:      " (reduce + points-sample1))

    (println "copies - sample2:   " copies-sample2)
    (println "scratchcards sample2" (solve2 text))

    ; solution
    (println "sum - part1: " (solve input))
    (println "sum - part2: " (solve2 input))
    ))
