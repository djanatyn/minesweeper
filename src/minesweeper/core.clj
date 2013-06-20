(ns minesweeper.core
  (:use [clojure.math.combinatorics :as c]))

(defn coords [[width height]]
  (mapcat (fn [x] (map (fn [y] [x y]) (range height))) (range width)))

(def minefield '[[b b b b b]
                 [b m b b b]
                 [b b m b b]
                 [b b b b b]
                 [b b b b b]])

(def tinyboard '[[b b m]
                 [b m b]
                 [b b m]])

(defn get-tile [board [x y]]
  (nth (nth board y) x))

(defn valid? [board [x y]]
  (let [width  (count (first board))
        height (count board)]
    (and (>= x 0) (>= y 0) (< x width) (< y height))))

(defn neighbors [grid [x y]]
  (let [delta-spaces [[-1 1] [0 1] [1 1] [-1 0] [1 0] [-1 -1] [0 -1] [1 -1]]
        surroundings (map #(map + [x y] %) delta-spaces)]
    (filter (partial valid? grid) surroundings)))

(defn get-value "returns either the number of surrounding mines or m if it's a mine"
  [board [x y]] 
  (case (get-tile board [x y])
    m 'm
    b (count (filter #(= 'm %) (map (partial get-tile board) (neighbors board [x y]))))))

(defn replace-tile "returns a new board with the requested tile replaced with a new value"
  [board [x y] replacement] 
  (assoc board y (assoc (nth board y) x replacement)))

(defn add-numbers [board]
  (let [height (count board)
        width  (count (first board))]
    (loop [new-board board
           tiles     (coords [height width])]
      (let [current-tile (first tiles)
            tiles-left   (rest tiles)]
        (if (empty? tiles) new-board
            (recur (replace-tile new-board current-tile (get-value board current-tile)) tiles-left))))))
