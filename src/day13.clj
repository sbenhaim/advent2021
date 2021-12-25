(ns day13
  (:require [clojure.string :as s]))

(defn parse-int [s]
  (Integer/parseInt s))

(def folds
  (->> "fold along x=655
fold along y=447
fold along x=327
fold along y=223
fold along x=163
fold along y=111
fold along x=81
fold along y=55
fold along x=40
fold along y=27
fold along y=13
fold along y=6"
       (re-seq #"(x|y)=(\d+)")
       (map (fn [[a b c]] [(keyword b) (parse-int c)]))))

(def coords
  (->> (slurp "src/day13-input.txt")
       (s/split-lines)
       (mapv #(s/split % #","))
       (mapv #(mapv parse-int %))))

(defn fold-val [val fold-point]
  (if (> val fold-point) (- fold-point (- val fold-point)) val))

(defn fold [coords [dir fold-point]]
  (if (= dir :x)
    (mapv (fn [[x y]] [(fold-val x fold-point) y]) coords)
    (mapv (fn [[x y]] [x (fold-val y fold-point)]) coords)))

;; Part 1

(count
 (set
  (fold coords [:x 655])))

;; Part 2

(defn print-coords [crds]
  (let [max-x (apply max (map first crds))
        max-y (apply max (map second crds))
        m (vec (repeat (inc max-y) (vec (repeat (inc max-x) " "))))]
    (println
     (s/join "\n" (->> crds
                       (reduce (fn [m [x y]] (assoc-in m [y x] "#")) m)
                       (map #(apply str %)))))))

(print-coords (set (reduce fold coords folds)))
