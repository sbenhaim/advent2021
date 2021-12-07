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

(defn day
  "Given today's fish, give me tomorrow's fish"
  [fish]
  (reduce (fn [tomorrow ^long n]
            (if (> n 0)
              (conj tomorrow (dec n)) ;; If n > 0, add a decremented n to tomorrow
              (conj tomorrow 6 8))) ;; Otherwise add 6 and 8 to tomorrow
          [] ;; Initialize tomorrow as an empty list
          fish))

(defn simulate
  "Given a list of fish timers and a number of days, simulate"
  [fish days]
  (if (= days 0)
    fish
    (recur (day fish) (dec days)))) ;; Tick fish timers and decrement the day

;; Part 1

(println
 (time
  (count (simulate input 80)))) ;; 166 msecs

;; Part 2

(def m-count-fish
  ;; Just tracking counts and no longer holding onto the simulation, as there will be lots of fish
  ;; Once we've solved for a starting timer / number of days combo
  ;; we don't need to do so again, thus memoize
  ;; Looking at only fish at a time, to improve memoization
  ;; We can then map that over a list of timers
  (memoize
   (fn [days timer]
     (cond (<= days timer) 1 ;; Once there are fewer days than the timer, the fish won't reproduce
           (= timer 0) (+ (m-count-fish (dec days) 6) (m-count-fish (dec days) 8)) ;; When the timer is 0, sum count of tomorrow's offspring ... recursively!
           :else (m-count-fish (dec days) (dec timer)))))) ;; Decrement the timer and the day

(println
 (time
  ;; Map over the initial timers individually and sum up the results
  (reduce + (map (partial m-count-fish 256) input)))) ;; 5.5 msecs
