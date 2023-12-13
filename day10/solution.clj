(ns day10.solution
  (:require [clojure.string :as str]))

(def neighbour-pos [[0 -1] [-1 0] [1 0] [0 1]])             ; N W E S

(def pipes {\F [[1 0] [0 1]]
            \| [[0 -1] [0 1]]
            \L [[1 0] [0 -1]]
            \- [[-1 0] [1 0]]
            \J [[-1 0] [0 -1]]
            \7 [[-1 0] [0 1]]})

(defn invert-coords [coords]
  (map #(* -1 %) coords))

(defn calc-pos [[x y] [x-rel y-rel]]
  [(+ x x-rel) (+ y y-rel)])

(defn equal-pos? [[x1 y1] [x2 y2]]
  (and (= x1 x2) (= y1 y2)))

(defn inside-grid? [[x y] x-dim y-dim]
  (and (>= x 0) (< x x-dim)
       (>= y 0) (< y y-dim)))

(defn in-pos-list? [pos-list [x y]]
  (some (fn [[x-list y-list]]
          (and (= x x-list) (= y y-list)))
        pos-list))

(defn pipe-tile? [tile]
  (some #(= % tile) (keys pipes)))

(defn reachable-pos? [pos tile nb]
  (let [rel-pos (if (= tile \S)
                  (map invert-coords (get pipes (:tile nb)))  ; take nb's rel-pos coords by inverting
                  (get pipes tile))
        nb-abs-pos (map #(calc-pos pos %) rel-pos)
        reachable-nbs (filter #(equal-pos? (:pos nb) %) nb-abs-pos)]
    (not (empty? reachable-nbs))))

(defn get-neighbours [lines pos]
  (let [x-dim (count (first lines))
        y-dim (count lines)]
    (reduce (fn [neighbours rel-pos]
              (let [[xn yn] (calc-pos pos rel-pos)]
                (if (inside-grid? [xn yn] x-dim y-dim)
                  (conj neighbours {:tile (nth (nth lines yn) xn)
                                    :pos [xn yn]})
                  neighbours)))
            []
            neighbour-pos)))

(defn find-start-pos [lines]
  (loop [y 0 pos nil]
    (if (and (< y (count lines)) (not pos))
      (recur (inc y)
             (let [line (nth lines y)
                   x (.indexOf line "S")]
               (if (>= x 0) [x y])))
      pos)))

(defn get-s-tile [pipe-loop]
  "determine the tile of 'S' by testing all pipe directions.
   the right one has to connect both neighbours of 'S'"
  (let [s-pos (first pipe-loop)
        nb1 (second pipe-loop)
        nb2 (last pipe-loop)]
    (reduce (fn [acc [tile [dir1 dir2]]]
              (if (or (and (equal-pos? nb1 (calc-pos s-pos dir1))
                           (equal-pos? nb2 (calc-pos s-pos dir2)))
                      (and (equal-pos? nb2 (calc-pos s-pos dir1))
                           (equal-pos? nb1 (calc-pos s-pos dir2))))
                tile
                acc))
            nil
            pipes)))

(defn get-pipe-nbs [lines pos]
  (filter #(pipe-tile? (:tile %))
          (get-neighbours lines pos)))

(defn move [lines cur-pos & [prev-pos]]
  (let [[x y] cur-pos
        tile (nth (nth lines y) x)
        pipes-nbs (get-pipe-nbs lines cur-pos)
        reachable-nbs (filter #(reachable-pos? cur-pos tile %) pipes-nbs)
        next-nbs (if prev-pos
                  (filter #(not (equal-pos? prev-pos (:pos %))) reachable-nbs)
                  reachable-nbs)]
    (if (not (empty? next-nbs))
      (:pos (first next-nbs)))))

(defn get-pipe-loop [lines]
  (loop [cur-pos (find-start-pos lines)
         prev-pos nil
         path []]
    (if cur-pos
      (recur (move lines cur-pos prev-pos)
             cur-pos
             (conj path cur-pos))
      path)))

(defn replace-outer-loop-elems [lines pipe-loop]
  (reduce (fn [acc [x y]]
           (let [x-dim (count (first lines))
                 tile (nth (nth lines y) x)]
             (str/join [acc
                        (if (in-pos-list? pipe-loop [x y]) tile \.)
                        (if (= x (dec x-dim)) "\n" "")])))
        ""
        (for [y (range 0 (count lines))
              x (range 0 (count (first lines)))] [x y])))

(defn count-inner-fields [lines]
  "counting all '.' fields if the wall count is odd. (because to be inside the loop one wall/pipe has to be crossed)
   counting all vertical walls crosses only, because we iterate each line horizontal:
     |  => cross
     L7 => cross
     FJ => cross
     LJ => no cross
     F7 => no cross"
  (-> (reduce (fn [acc [x y]]
                (let [x-dim (count (first lines))
                      tile (nth (nth lines y) x)
                      walls (if (or (= tile \|)
                                    (and (= tile \7) (= (:last-wall acc) \L))
                                    (and (= tile \J) (= (:last-wall acc) \F)))
                              (inc (:walls acc))
                              (:walls acc))
                      last-wall (cond
                                  ; wall start
                                  (or (= tile \F) (= tile \L)) tile

                                  ; wall crossed
                                  (and (= tile \7) (= (:last-wall acc) \L)) nil
                                  (and (= tile \J) (= (:last-wall acc) \F)) nil

                                  ; wall not crossed
                                  (and (= tile \7) (= (:last-wall acc) \F)) nil
                                  (and (= tile \J) (= (:last-wall acc) \L)) nil

                                  ; do nothing
                                  :else (:last-wall acc))
                      inner (if (and (= tile \.) (odd? walls))
                              (inc (:inner acc))
                              (:inner acc))]
                  (assoc acc :walls (if (= x (dec x-dim)) 0 walls)
                             :last-wall (if (= x (dec x-dim)) nil last-wall)
                             :inner inner)))
              {:inner 0 :walls 0 :last-wall nil}
              (for [y (range 0 (count lines))
                    x (range 0 (count (first lines)))] [x y]))
      :inner))


(defn solve1 [input]
  (let [lines (str/split-lines input)
        pipe-loop (get-pipe-loop lines)]
    (int (Math/ceil (/ (count pipe-loop) 2)))))

(defn solve2 [input]
  (let [lines (str/split-lines input)
        pipe-loop (get-pipe-loop lines)
        cleared-grid-input (replace-outer-loop-elems lines pipe-loop)]
    (count-inner-fields (str/split-lines
                          (str/replace cleared-grid-input \S (get-s-tile pipe-loop))))))

(defn main []
  (let [text (slurp "day10/sample-input.txt")
        text2 (slurp "day10/sample-input2.txt")
        text3 (slurp "day10/sample-input3.txt")
        text4 (slurp "day10/sample-input4.txt")
        text5 (slurp "day10/sample-input5.txt")
        input (slurp "day10/input.txt")]

    ; sample
    (println "max distance to 'S' - sample 1: " (solve1 text))
    (println "max distance to 'S' - sample 1 (complex): " (solve1 text2))

    (println "enclosed tiles sum - sample3: " (solve2 text3))
    (println "enclosed tiles sum - sample4: " (solve2 text4))
    (println "enclosed tiles sum - sample5: " (solve2 text5))

    ; solution
    (println "result - part 1: " (solve1 input))
    (println "result - part 2: " (solve2 input))
    ))