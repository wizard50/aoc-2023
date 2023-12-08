(ns day05.solution
  (:require [clojure.string :as str]))

(defn parse-section [section-text]
  (->> (str/split-lines section-text)
       (drop 1)
       (map (fn [line]
              (map parse-long (re-seq #"\d+" line))))))

(defn parse [input]
  (let [sections (str/split input #"\n\n")
        seeds (map parse-long (re-seq #"\d+" (first sections)))
        maps (map parse-section
                  (drop 1 sections))]
    {:seeds seeds :maps maps}))

(defn get-seed-range [seeds]
  (loop [i 0 result []]
    (if (< (+ i 1) (count seeds))
      (let [start (nth seeds i)
            len (nth seeds (inc i))
            range [start (dec (+ start len))]
            new-result (conj result range)]
        (recur (+ i 2) new-result))
      result)))

(defn get-destination-value [conversion-row value]
  (let [[destination-range-start
         source-range-start
         range-length] conversion-row
        destination? (and (>= value source-range-start) (< value (+ source-range-start range-length)))]
    (if destination?
      (let [offset (- value source-range-start)]
        (+ destination-range-start offset)))))

(defn convert-value [value conversion-map]
  "if the destination-value could not be found its the same as the source value"
  (loop [i 0
         destination-value nil]
    (if (and (< i (count conversion-map)) (not destination-value))
        (recur
          (inc i)
          (get-destination-value (nth conversion-map i) value))
      (if destination-value
        destination-value
        value))))

(defn resolve-location [seed maps]
  (reduce convert-value seed maps))

(defn get-destination-range [conversion-row range]
  "possible cases
    - no intersection
    - range completely inside
    - right intersection
    - left intersection
    - overlap / superset"
  (let [[destination-range-start source-range-start range-length] conversion-row
        [start end] range
        intersection-start (cond
                             ; start inside
                             (and (>= start source-range-start)
                                  (< start (+ source-range-start range-length))) start
                             ; end inside / overlap
                             (and (< start source-range-start)
                                  (>= end source-range-start)) source-range-start
                             :else nil)
        intersection-end    (cond
                              ; end inside
                              (and (>= end source-range-start)
                                   (< end (+ source-range-start range-length))) end
                              ; start inside / overlap
                              (and (< start (+ source-range-start range-length))
                                   (>= end (+ source-range-start range-length))) (+ source-range-start range-length -1)
                              :else nil)]
    (if intersection-start
      (let [start-offset (- intersection-start source-range-start)
            end-offset (- intersection-end source-range-start)
            intersection-range [(+ destination-range-start start-offset) (+ destination-range-start end-offset)]
            remaining-range-left (if (< start source-range-start)
                                   [(- source-range-start (- source-range-start start)) (dec source-range-start)])
            remaining-range-right (if (>= end (+ source-range-start range-length))
                                    [(+ source-range-start range-length) end])]
        [intersection-range (filter #(not (empty? %))
                                    (conj [] remaining-range-left remaining-range-right))])
      [nil [range]])))

(defn convert-ranges [ranges conversion-map]
  (let [result (reduce (fn [result row]
                  (reduce (fn [acc range]
                            ; intersection-range is converted. remaining-ranges need further processing, kept as is
                            (let [[intersection-range remaining-ranges] (get-destination-range row range)]
                              {:remaining-ranges (if remaining-ranges
                                                   (concat (:remaining-ranges acc) remaining-ranges)
                                                   (:remaining-ranges acc))
                               :intersection-ranges (if intersection-range
                                                      (conj (:intersection-ranges acc) intersection-range)
                                                      (:intersection-ranges acc))}))
                            {:remaining-ranges [] :intersection-ranges (:intersection-ranges result)}
                            (:remaining-ranges result)))
              {:remaining-ranges ranges :intersection-ranges []}
              conversion-map)]
    (if (empty? (:remaining-ranges result))
      (:intersection-ranges result)
      (concat (:intersection-ranges result) (:remaining-ranges result)))))

(defn resolve-location-range [seeds maps]
  (reduce convert-ranges seeds maps))

(defn solve [input]
  (let [data (parse input)]
       (->> (map #(resolve-location % (:maps data)) (:seeds data))
            (reduce min))))

(defn solve2 [input]
  (let [data (parse input)
        seeds (get-seed-range (:seeds data))]
    (->> (map #(resolve-location-range [%] (:maps data)) seeds)
         (map #(reduce (fn [acc range]
                         (if acc
                           (min acc (first range))
                           (first range))) nil %))
         (reduce min)
         )))

(defn main []
  (let [text (slurp "day05/sample-input.txt")
        input (slurp "day05/input.txt")
        sample-data (parse text)]

    ; sample
    (println "convert seed to soil - sample1" (map #(convert-value % (first (:maps sample-data))) (:seeds sample-data)))
    (println "resolve seed to location - sample 1" (map #(resolve-location % (:maps sample-data)) (:seeds sample-data)))
    (println "min location - sample 1" (solve text))

    (println "seed range - sample 2: " (get-seed-range (:seeds sample-data)))
    (println "seed range sum - sample 2: " (count (get-seed-range (:seeds sample-data))))
    (println "min location - sample 2: " (solve2 text))

    ; solution
    (println "min location - part1: " (solve input))
    (println "min location - part2: " (solve2 input))
    ))
