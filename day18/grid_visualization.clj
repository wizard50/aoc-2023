(ns day18.grid-visualization
  (:require [clojure.string :as str]))

(defn get-start-pos [prev-pos dir]
  (if prev-pos
    (let [[x y] prev-pos]
      (case dir
        "R" [(inc x) y]
        "L" [(dec x) y]
        "D" [x (inc y)]
        "U" [x (dec y)]))
    [0 0]))

(defn get-end-pos [start-pos dir n]
  (let [[x y] start-pos]
    (case dir
      "R" [(+ x (dec n)) y]
      "L" [(- x (dec n)) y]
      "D" [x (+ y (dec n))]
      "U" [x (- y (dec n))])))

(defn parse-dig-plan [input]
  (let [lines (str/split-lines input)]
    (loop [i 0
           pos nil
           items []]
      (if (< i (count lines))
        (let [line (nth lines i)
              [dir n rgb] (str/split line #" ")
              n (parse-long n)
              start-pos (get-start-pos pos dir)
              end-pos (get-end-pos start-pos dir n)
              item {:dir dir
                    :n n
                    :rgb (-> rgb
                             (str/replace #"\(#" "")
                             (str/replace #"\)" ""))
                    :start-pos start-pos
                    :end-pos end-pos}]
          (recur
            (inc i)
            end-pos
            (conj items item)))
        items))))

(defn get-min-max-indices [dig-plan]
  "compare x and y values of all start and end positions and determine the min and max indices"
  (reduce (fn [acc d]
            (let [{:keys [start-pos end-pos]} d
                  {:keys [min-x min-y max-x max-y]} acc
                  [x1 y1] start-pos
                  [x2 y2] end-pos]
              (assoc acc :min-x (min min-x x1 x1)
                         :min-y (min min-y y1 y2)
                         :max-x (max max-x x1 x2)
                         :max-y (max max-y y1 y2))))
          {:min-x 0
           :min-y 0
           :max-x 0
           :max-y 0}
          dig-plan))

(defn dimension [dig-plan]
  (let [{:keys [min-x min-y max-x max-y]} (get-min-max-indices dig-plan)]
    {:x (+ (abs max-x) (abs min-x) 1)
     :y (+ (abs max-y) (abs min-y) 1)
     :x-offset (abs min-x)
     :y-offset (abs min-y)}))

(defn replace-at [s idx replacement]
  (str (subs s 0 idx) replacement (subs s (inc idx))))

(defn list-update-at [list idx item]
  (vec (concat (take idx list) [item] (drop (inc idx) list))))

(defn draw-h-line [grid [x y] n x-off y-off]
  (let [line (nth grid (+ y y-off))
        mod-line (str (subs line 0 (+ x x-off))
                      (str/join (repeat n "-"))
                      (subs line (+ x n x-off)))]
    (list-update-at grid (+ y y-off) mod-line)))

(defn draw-v-line [grid [x y1] [_ y2] x-off y-off]
  (map (fn [i]
         (let [line (nth grid i)
               y-from (+ (min y1 y2) y-off)
               y-to (+ (max y1 y2) y-off)]
           (if (and (>= i y-from) (<= i y-to))
             (replace-at (nth grid i) (+ x x-off) "|")
             line)))
       (range 0 (count grid))))

(defn dig [grid dim dig-plan]
  (reduce #(let [{:keys [dir n start-pos end-pos]} %2
                 {:keys [x-offset y-offset]} dim]
             (case dir
               "R" (draw-h-line %1 start-pos n x-offset y-offset)
               "L" (draw-h-line %1 end-pos n x-offset y-offset)
               "D" (draw-v-line %1 start-pos end-pos x-offset y-offset)
               "U" (draw-v-line %1 start-pos end-pos x-offset y-offset)
               %1))
          grid
          dig-plan))


(defn create-grid [input]
  (let [dig-plan (parse-dig-plan input)
        dim (dimension dig-plan)
        {:keys [x y]} dim
        empty-grid (repeat y (str/join (repeat x ".")))]
    (dig empty-grid dim dig-plan)))

(defn print-grid [grid]
  (println (str/join "\n" grid)))

(defn main []
  (let [text (slurp "day18/sample-input.txt")
        input (slurp "day18/input.txt")]

    ; sample
    (print-grid (create-grid text))

    ; solution
    (print-grid (create-grid input))
    ))