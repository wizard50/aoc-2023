(ns day12.solution
  (:require [clojure.string :as str]))


(defn parse [input]
  (->> (str/split-lines input)
       (map #(let [[records groups] (str/split % #" ")]
               {:records records
                :groups (map parse-long (str/split groups #","))}))))

(def fsm-mem
  (memoize (fn fsm [records groups & [consumed]]
             (loop [i 0
                    j 0
                    state :START
                    consumed (if consumed consumed 0)
                    result nil]
               (if (and (< i (count records)) (not result))
                 (let [ch (nth records i)
                       groups-finished (= (count groups) j)
                       group-size (if (< j (count groups))
                                    (nth groups j)
                                    0)
                       next-state (case state
                                    :START (case ch
                                             \# :CONSUME
                                             \. :DOT
                                             \? (if groups-finished
                                                  :DOT                   ; has to be '.' (all groups are finished, '#' not expected anymore)
                                                  :R2))                  ; 2 possible results. start 2 new fms

                                    :CONSUME (case ch
                                               \# (cond
                                                    groups-finished :R0
                                                    (< (inc consumed) group-size) :CONSUME                              ; group not finished => continue
                                                    (= (inc consumed) group-size) :DOT)                                 ; single group is finished, '.' is expected next
                                               \? (cond
                                                    groups-finished :R0
                                                    (< (inc consumed) group-size) :CONSUME                              ; group not finished => continue
                                                    (= (inc consumed) group-size) :DOT)                                 ; single group is finished, '.' is expected next
                                               \. :R0)  ; error if '.' comes before the group is finished

                                    :DOT (case ch
                                           \. :START
                                           \? :START
                                           \# :R0))                    ; error, '#' not expected here
                       next-consumed (if (= state :CONSUME)
                                       (if (< (inc consumed) group-size)
                                         (inc consumed)
                                         0)
                                       consumed)
                       next-j (if (and (= state :CONSUME) (= (inc consumed) group-size))
                                (inc j)
                                j)
                       result (case next-state
                                :R0 0
                                :R1 1
                                :R2 (+ (fsm-mem (str \# (subs records (inc i))) (nthrest groups j) consumed)
                                       (fsm-mem (str \. (subs records (inc i))) (nthrest groups j) consumed))
                                nil)]
                   (recur (if (= state :START) i (inc i))              ; :START state does not consume chars
                          next-j
                          next-state
                          next-consumed
                          result))
                 (if result
                   result
                   (if (and (= i (count records)) (= j (count groups)))
                     1
                     0)))))))

(defn solve1 [input]
  (->> (parse input)
       (map #(fsm-mem (:records %) (:groups %)))
       (reduce +)))

(defn solve2 [input]
  (->> (parse input)
       (map #(fsm-mem (str/join "?" (repeat 5 (:records %)))
                  (reduce concat (repeat 5 (:groups %)))))
       (reduce +)))

(defn main []
  (let [text (slurp "day12/sample-input.txt")
        input (slurp "day12/input.txt")]

    ; sample
    (println "sample1:" "???.###" [1,1,3] "=>" (fsm-mem "???.###" [1,1,3]))
    (println "sample1:" ".??..??...?##." [1 1 3] "=>" (fsm-mem ".??..??...?##." [1 1 3]))
    (println "sample1:" "?#?#?#?#?#?#?#?" [1 3 1 6] "=>" (fsm-mem "?#?#?#?#?#?#?#?" [1 3 1 6]))
    (println "sample1:" "????.#...#..." [4 1 1] "=>" (fsm-mem "????.#...#..." [4 1 1]))
    (println "sample1:" "????.######..#####." [1 6 5] "=>" (fsm-mem "????.######..#####." [1 6 5]))
    (println "sample1:" "?###????????" [3 2 1] "=>" (fsm-mem "?###????????" [3 2 1]))

    (println "result - sample 1: " (solve1 text))
    (println "result - sample 2: " (solve2 text))

    ; solution
    (println "result - part1: " (solve1 input))
    (println "result - part2: " (solve2 input))
    ))
