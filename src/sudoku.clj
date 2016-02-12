(ns sudoku
  (:require [clojure.set :as set]))

(def board identity)
(def all-values #{1 2 3 4 5 6 7 8 9})

(defn value-at [board coord]
  (get-in board coord))

(defn has-value? [board coord]
  (contains? all-values (value-at board coord)))

(defn row-values [board coord]
  (let [[row col] coord]
    (set (get board row))))

(defn col-values [board coord]
  (let [[row col] coord]
    (set (for [r board]
           (get r col)))))


(defn coord-pairs [coords]
  (for [x coords
        y coords]
    [x y]))

(defn block-values [board coord]
  (let [[x y] coord
        row (int (/ x 3))
        col (int (/ y 3))]
    (map (fn [[a b]]
           [(+ a row) (+ a col)])
         (coord-pairs [0 1 2]))))

(defn valid-values-for [board coord]
  (if (= (value-at board coord) 0)
    (set/difference all-values
                    (row-values board coord)
                    (col-values board coord)
                    (for [x (block-values board coord)]
                      (value-at (board x))))
    #{}))

(defn filled? [board]
  (empty? (filter (fn [x] (not (= x 0)))
                  (map (fn [x] (value-at board x))
                       (apply list (coord-pairs [0 1 2 3 4 5 6 7 8]))))))

(defn rows [board]
  (map (fn [x] (row-values x))
       [0 1 2 3 4 5 6 7 8]))

(defn valid-rows? [board]
  (empty? (filter (fn [x] (not (= x all-values)))
                  (rows board))))

(defn cols [board]
  (map (fn [x] (col-values x))
       [0 1 2 3 4 5 6 7 8]))

(defn valid-cols? [board]
  (empty? (filter (fn [x] (not (= x all-values)))
                  (cols board))))

(defn blocks [board]
  (map (fn [x] (block-values))
       (coord-pairs [0 3 6])))

(defn valid-blocks? [board]
  (empty? (filter (fn [x] (not (= x all-values)))
                  (blocks board))))

(defn valid-solution? [board]
  (and (valid-rows? board)
       (valid-cols? board)
       (valid-blocks? board)))

(defn set-value-at [board coord new-value]
  (assoc-in board coord new-value))

(defn find-empty-point [board]
  (first (filter (fn [coord] (= 0 (value-at board coord)))
                 (coord-pairs [0 1 2 3 4 5 6 7 8]))))

(defn solve [board]
  nil)

(defn solve-helper [board current-board]
  (if (valid-solution? current-board)
    current-board
    (let [

