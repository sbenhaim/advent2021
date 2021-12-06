(ns day4
  (:require [clojure.string :as s]
            [clojure.set :refer [union subset? difference]]))

(defn parse-int [s]
  (Integer/parseInt s))

(def input (slurp "src/day4-input.txt"))

(def draws
  (-> input
      (s/split-lines)
      first
      (s/split #",")))

(defn board->bingos
  "Convert a bingo board into the set of all bingos,
  which is a union of the set of rows and the set of columns"
  [board]
  (union
   (set (map set board))
   (set (map set (apply map list board)))))

(def boards
  (let [text-boards (-> input
                        (s/split #"\n\n")
                        rest)
        board-rows (mapv #(s/split-lines %) text-boards)
        board-vectors (mapv (fn [rows] (mapv #(vec (re-seq #"\d+" %)) rows)) board-rows)]
    (map board->bingos board-vectors)))


(defn bingo?
  "Given called numbers `ns`, is this board a bingo?"
  [ns board]
  (let [sns (set ns)]
    ;; Are any of the board bingos a subset of the called numbers?
    (some true? (map #(subset? % sns) board))))

(defn winners
  "Find the first of `boards` to get bingo from `draws` by
  starting with 5 draws (min for bingo) and increasing until one or
  more winners are found"
  [draws boards]
  (loop [n 5]
    (let [called (take n draws)
          bingos (filter #(bingo? called %) boards)]
      (if (seq bingos) [called bingos]
          (recur (inc n))))))

(defn score [called winner]
  (let [last-called (parse-int (last called))
        ;; Unmarked are the numbers not called (i.e., set diff of board numbers and called numbers)
        unmarked (map parse-int (difference (set (apply union winner)) (set called)))]
    (* last-called (apply + unmarked))))

;; Part 1

(println
 (let [[draws winners] (winners draws boards)]
   (score draws (first winners))))

;; Part 2

(println
 (loop [boards (set boards)] ;; Starting with the set of all boards
   (let [[draws winners] (winners draws boards) ;; Find the winners
         winners (set winners)]
     (if (= winners boards) ;; If the winner(s) are the only boards left
       (score draws (first winners)) ;; They are the last, so score the first of the last
       (recur (difference boards winners)))))) ;; Otherwise try again after removing the winning boards
