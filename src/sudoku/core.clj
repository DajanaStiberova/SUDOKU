(ns sudoku.core
  (:require [clojure.pprint :refer :all]))

(def board
  {[0 0] (into [] (map (partial into [])
                       (partition 3 (shuffle (range 1 10)))))})

(comment
  {[0 0] [[9 2 4]
          [6 1 3]
          [5 8 7]]})

(defn one-nine-rand-without [& exclusions]
  (let [random-ranges (->> exclusions
                           (cons 0)
                           (cons 10)
                           set
                           sort
                           (partition 2 1)
                           (filter (fn [[lower upper]]
                                     (> (- upper lower) 1))))
        [lower upper] (nth random-ranges (rand-int (count random-ranges)))]
    (+ (rand-int (- (dec upper) lower))
       (inc lower))))

(defn prime []
  (assoc board [0 1]
         ()))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NEW SUDOKU ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def start-board
  {1 {:R1 [1 2 3 4 5 6 7 8 9]
      :R2 [4 5 6 7 8 9 1 2 3]
      :R3 [7 8 9 1 2 3 4 5 6]}
   2 {:R4 [2 3 4 5 6 7 8 9 1]
      :R5 [5 6 7 8 9 1 2 3 4]
      :R6 [8 9 1 2 3 4 5 6 7]}
   3 {:R7 [9 1 2 3 4 5 6 7 8]
      :R8 [3 4 5 6 7 8 9 1 2]
      :R9 [6 7 8 9 1 2 3 4 5]}})

(defn- swap [section]
  (->> section
       seq
       shuffle
       (into {})))

(defn- get-changed-numbers [board]
  (let [change-numbers (zipmap (shuffle (range 1 10)) (shuffle (range 1 10)))]
    (map (fn [row]
           (replace change-numbers row))
         (reduce into [] (map vals (vals board))))))

(defn swap-board [board]
  (-> board
      (update-in [1] swap)
      (update-in [2] swap)
      (update-in [3] swap)
      swap))

(defn rows->columns [board]
  (let [new-board (get-changed-numbers board)]
    (zipmap '(1 2 3)
            (->> (map (fn [[r c]]
                        (assoc {} r (->> (map (fn [row]
                                                (nth row c))
                                              new-board)
                                         (into []))))
                      (zipmap (sort (reduce into [] (map keys (vals board)))) (range 0 9)))
                 (partition 3)
                 (map (fn [section]
                        (reduce into {} section)))))))

(defn final-board []
  (->> start-board
       swap-board
       rows->columns
       swap-board
       rows->columns))
