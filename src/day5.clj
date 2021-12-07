(ns day5
  (:require [clojure.string :as s]))

(def input
  (s/split-lines (slurp "src/day5-input.txt")))

(defn parse-int [s]
  (Integer/parseInt s))

(def lines
  (->> input
       (mapv (comp #(re-seq #"\d+" %)))
       (mapv (fn [ns] (mapv parse-int ns)))))

(defn is-h-or-v? [[x1 y1 x2 y2]]
  (or (= x1 x2)
      (= y1 y2)))

(defn bi-range
  "Inclusive range function that gives a reverse range if a > b"
  [a b]
  (if (> b a)
    (range a (inc b))
    (range a (dec b) -1)))


(defn line-points
  "Returns all coordinates incuded in a horizontal or vertical line"
  [[x1 y1 x2 y2]]
  (for [x (bi-range x1 x2)
        y (bi-range y1 y2)]
    [x y]))

(->> lines
     (filter is-h-or-v?)
     (mapcat line-points) ;; Flat list of all coordinates of all lines
     frequencies ;; Frequence for each coord
     (filter (fn [[_ n]] (> n 1))) ;; Filter by frequency > 0
     count)

;; Part 1
(->> lines
     (filter is-h-or-v?)
     (mapcat line-points)
     frequencies
     (filter (fn [[_ n]] (> n 1)))
     count)


;; Part 2
(defn line-points2
  "Returns all coordinates included in horizontal, vertical or diagonal line"
  [[x1 y1 x2 y2]]
  (let [xs (bi-range x1 x2)
        ys (bi-range y1 y2)]
    (if (or (= x1 x2) (= y1 y2))
      (for [x xs y ys]
        [x y])
      (map #(vector %1 %2) xs ys))))

(->> lines
     (mapcat line-points2)
     frequencies
     (filter (fn [[_ n]] (> n 1)))
     count)
