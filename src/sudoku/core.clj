(ns sudoku.core
  (:require [clojure.set :as set]
            [clojure.pprint :refer :all]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; UTILS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn zero-eight-rand-without [& exclusions]
  (let [random-ranges (->> exclusions
                           (cons -1)
                           (cons 9)
                           set
                           sort
                           (partition 2 1)
                           (filter (fn [[lower upper]]
                                     (> (- upper lower) 1))))
        [lower upper] (nth random-ranges (rand-int (count random-ranges)))]
    (+ (rand-int (- (dec upper) lower))
       (inc lower))))

(defn transpose [matrix]
  (mapv (fn [idx]
          (mapv #(nth % idx) matrix))
        (range 0 (count matrix))))

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

(defn final-board [board]
  (->> board
       swap-board
       rows->columns
       swap-board
       rows->columns
       vals
       (mapcat vals)
       (into [])))

;;;;;;;;;;;;;;;;;;;;;;;; REMOVE NUMBERS ;;;;;;;;;;;;;;;;;;;;;

(defn remove-number [board]
  (let [board board
        row (rand-int 9)
        second-row (- 8 row)
        index (rand-int 9)
        second-index (- 8 index)]
    (-> board
        (assoc-in [row index] nil)
        (assoc-in [second-row second-index] nil))))

(defn remove-numbers [board]
  (loop [i 0
         b board]
    (if (= i 19)
      b
      (recur (inc i)
             (remove-number b)))))

;;;;;;;;;;;;;;;;;;;;;;;; FINAL GENERATE ;;;;;;;;;;;;;;;;;;;;;

(def generate (comp remove-numbers final-board))

;;;;;;;;;;;;;;;;;;;;;;;; SOLVER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def ^:private full-set #{1 2 3 4 5 6 7 8 9})

(defn candidates [series]
  (-> series set (disj nil) (as-> row-set (set/difference full-set row-set))))

(defn choose-candidate [row column]
  (let [final-candidates (set/intersection (candidates row)
                                           (candidates column))]
    (when (= 1 (count final-candidates))
      (first final-candidates))))

(defn solver-pass [board]
  (let [colls (transpose board)]
    (mapv (fn [row]
            (mapv (fn [number idx]
                    (if number
                      number
                      (choose-candidate row (nth colls idx))))
                  row (range 0 9)))
          board)))


(def hard-board [[nil 9 nil 3 nil 1 nil 4 nil nil]
                 [nil 1 nil nil 4 nil nil nil nil]
                 [2 4 nil nil 9 7 nil 1 nil]
                 [7 8 9 nil 1 nil nil nil nil]
                 [4 2 nil nil nil nil nil nil 1]
                 [nil 3 1 4 nil 2 7 nil 9]
                 [1 nil 8 nil nil nil 3 nil nil]
                 [3 nil 4 1 nil nil nil 5 2]
                 [9 nil 2 nil nil nil 1 7 nil]])
