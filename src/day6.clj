(ns day6)

(def input
[4 1 1 4 1 2 1 4 1 3 4 4 1 5 5 1 3 1 1 1 4 4 3 1 5 3 1 2 5 1 1 5 1 1 4 1 1 1 1 2
1 5 3 4 4 1 1 1 1 1 1 1 1 1 2 1 1 1 1 1 5 1 1 1 4 1 2 3 5 1 2 2 4 1 4 4 4 1 2 5
1 2 1 1 1 1 1 1 4 1 1 4 3 4 2 1 3 1 1 1 3 5 5 4 3 4 1 5 1 1 1 2 2 1 3 1 2 4 1 1
3 3 1 3 3 1 1 3 1 5 1 1 3 1 1 1 5 4 1 1 1 1 4 1 1 3 5 4 3 1 1 5 4 1 1 2 5 4 2 1
4 1 1 1 1 3 1 1 1 1 4 1 1 1 1 2 4 1 1 1 1 3 1 1 5 1 1 1 1 1 1 4 2 1 3 1 1 1 2 4
2 3 1 4 1 2 1 4 2 1 4 4 1 5 1 1 4 4 1 2 2 1 1 1 1 1 1 1 1 1 1 1 4 5 4 1 3 1 3 1
1 1 5 3 5 5 2 2 1 4 1 4 2 1 4 1 2 1 1 2 1 1 5 4 2 1 1 1 2 4 1 1 1 1 2 1 1 5 1 1
2 2 5 1 1 1 1 1 2 4 2 3 1 2 1 5 4 5 1 4])

(defn day [fish]
  (reduce (fn [tomorrow ^long n]
            (if (> n 0)
              (conj tomorrow (dec n))
              (conj tomorrow 6 8)))
          []
          fish))

(defn simulate [fish days]
  (if (= days 0)
    fish
    (recur (day fish) (dec days))))

;; Part 1

(println
 (time
  (count (simulate input 80)))) ;; 166 msecs

;; Part 2

(def m-count-fish
  ;; Just tracking counts and no longer holding onto the simulation, as there will be lots of fish
  ;; Once we've solved for a starting timer / number of days combo
  ;; we don't need to do so again, thus memoize
  (memoize
   (fn [days timer]
     (cond (<= days timer) 1
           (= timer 0) (+ (m-count-fish (dec days) 6) (m-count-fish (dec days) 8))
           :else (m-count-fish (dec days) (dec timer))))))

(println
 (time
  (reduce + (pmap (partial m-count-fish 256) input)))) ;; 20 msecs
