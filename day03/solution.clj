(ns day03.solution
  (:require [clojure.string :as str]))

(def upper-neighbour-pos [[-1 -1] [0 -1] [1 -1]])
(def mid-neighbour-pos   [[-1 0]  [1 0]])
(def lower-neighbour-pos [[-1 1] [0 1] [1 1]])
(def all-neighbour-pos   (concat upper-neighbour-pos
                                 mid-neighbour-pos
                                 lower-neighbour-pos))

(defn get-neighbours [x y context]
  (let [{:keys [x-dim y-dim lines pos]} context]
    (reduce (fn [neighbours p]
       (let [[xi yi] p
             xn (+ x xi)
             yn (+ y yi)]
         (if (and (>= xn 0) (< xn x-dim)
                  (>= yn 0) (< yn y-dim))
          (conj neighbours (nth (nth lines yn) xn))
          neighbours)
         )) [] pos)
    ))

(defn collect-numbers [lines]
  (let [x-dim (count (first lines))
        y-dim (count lines)
        numbers (for [iy (range 0 y-dim)]
                  (loop [ix 0
                         numbers []
                         line (nth lines iy)]
                    (if (< ix x-dim)
                      (let [ch (nth line ix)
                            number-str (if (Character/isDigit ch)
                                         (str/join
                                           (take-while #(Character/isDigit %) (subs line ix))))
                            steps (if number-str (count number-str) 1)
                            neighbours (if number-str
                                         (->> (map #(get-neighbours % iy {:lines lines
                                                                          :x-dim x-dim
                                                                          :y-dim y-dim
                                                                          :pos all-neighbour-pos}) (range ix (+ ix steps)))
                                               (reduce concat)
                                               (distinct)))
                            adjacent? (if neighbours
                                        (reduce (fn [res n]
                                                  (if (not (str/includes? "0123456789." (str n))) true res))
                                                false neighbours))
                            next-ix (+ ix steps)
                            next-numbers (if adjacent?
                                           (conj numbers (Integer/parseInt number-str))
                                           numbers)]
                        (recur next-ix next-numbers line))
                      numbers)))
        numbers (reduce concat numbers)]
    numbers
  ))

(defn find-whole-number [x line]
  (let [r-part (str/join
                  (take-while #(Character/isDigit %) (subs line x)))
        l-part (-> (take-while #(Character/isDigit %) (str/reverse (subs line 0 (inc x))))
                   (str/join)
                   (str/reverse))]
    (if (and (not (empty? r-part))
             (not (empty? r-part)))
      (str/join [l-part (subs r-part 1)])
      (str/join [l-part r-part]))))

(defn get-neighbour-numbers-for-line [x line neighbour-fields rel-y]
  "a field can have up to two neighbour numbers per line"
  (if (or
        ; center row
        (and (= rel-y 0) (= (count neighbour-fields) 2))
        ; lower / upper row with 3 fields
        (and (= (count neighbour-fields) 3)
           (not (Character/isDigit (second neighbour-fields)))))
    ; 2 numbers possible (center field is not a number)
    (let [num1 (find-whole-number (dec x) line)
          num2 (find-whole-number (inc x) line)]
      (filter #(not (empty? %)) [num1 num2]))

    ; else 1 number possible
    (let [x-list (case (count neighbour-fields)
                   3 [(dec x) x (inc x)]
                   2 (if (= x 0)
                         [x (inc x)]     ; upper/lower row - x on left border
                         [(dec x) x])    ; upper/lower row - x on right border
                   1 (if (= x 0)
                       [(inc x)]     ; upper/lower row - x on left border
                       [(dec x)]     ; upper/lower row - x on right border
                  ))
          num-list (map #(find-whole-number % line) x-list)]
      (take 1
            (filter #(not (empty? %)) num-list)))))

(defn collect-numbers2 [iy context]
  (let [{:keys [x-dim y-dim lines]} context
        line (nth lines iy)
        numbers (for [ix (range 0 x-dim)]
                  (let [c (subs line ix (inc ix))
                        ; separate neighbours for upper, mid and lower row
                        neighbour-fields (if (= c "*")
                                           (->> (map #(get-neighbours ix iy {:lines lines
                                                                             :x-dim x-dim
                                                                             :y-dim y-dim
                                                                             :pos %}) [upper-neighbour-pos
                                                                                       mid-neighbour-pos
                                                                                       lower-neighbour-pos])))
                        lines3 (map (fn [i]
                                      (if (empty? (nth neighbour-fields i))
                                        []
                                        (case i
                                          0 (nth lines (dec iy))
                                          1 line
                                          2 (nth lines (inc iy))))) (range 0 3))]
                    (->> (if neighbour-fields
                           (map #(if (not (empty? %1))
                                   (get-neighbour-numbers-for-line ix %1 %2 (case %3
                                                                               0 -1
                                                                               1 0
                                                                               2 1))
                                   ())
                                lines3 neighbour-fields (range 0 3)))
                         (reduce concat))))]
    (filter #(> (count %) 0)  numbers)))

(defn gear-ratios [lines]
  (let [x-dim (count (first lines))
        y-dim (count lines)
        gear-ratios (for [iy (range 0 y-dim)]
                      (collect-numbers2 iy {:lines lines :x-dim x-dim :y-dim y-dim}))]
    (->> gear-ratios
         (reduce concat)
         (filter #(not (empty? %)))
         (filter #(= (count %) 2))
         (map (fn [pair]
                [(Integer/parseInt (first pair))
                 (Integer/parseInt (second pair))])))))

(defn gear-ratio-products [gear-ratios]
  (map #(* (first %) (second %))
         gear-ratios))

(defn main []
  (let [text (slurp "day03/sample-input.txt")
        input (slurp "day03/input.txt")
        sample-numbers1 (collect-numbers (str/split-lines text))
        sample-numbers2 (gear-ratios (str/split-lines text))
        sample-products (gear-ratio-products sample-numbers2)
        numbers1 (collect-numbers (str/split-lines input))
        numbers2 (->> (gear-ratios (str/split-lines input))
                      (gear-ratio-products))]

    ; sample
    (println "engine schematic - sample1: " sample-numbers1)
    (println "sum sample1" (reduce + sample-numbers1))

    (println "gear ratios - sample2: " sample-numbers2)
    (println "gear ratio products - sample2: " sample-products)
    (println "sum - sample2: " (reduce + sample-products))

    ; solution
    (println "sum - part1: " (reduce + numbers1))
    (println "sum - part2: " (reduce + numbers2))
    ))
