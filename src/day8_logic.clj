(ns day8-logic
  (:require [clojure.string :as s]
            [clojure.core.logic :refer [run* fresh membero == run everyg]]
            [clojure.core.logic.fd :as fd]))

(def signals (s/split "bceadfg ebcdaf gdaecf fgcaeb fdbea fcdea cbda begdf afb ba" #" "))

(def digits
  (->>
   (s/split "acfbeg cbfgea gebacf ab" #" ")
   (map #(s/split % #""))
   (map set)))



(let [[zero one two three four five six seven eight nine]
      (->>
       (run 1 [zero one two three four five six seven eight nine]
         (fresh [digits a b c d e f g]
           (== one [c f])
           (== seven [a c f])
           (== four [b c d f])
           (== eight [a b c d e f g])

           (== three [a c d f g])
           (== two [a c d e g])
           (== five [a b d f g])

           (== six [a b d e f g])
           (== nine [a b c d f g])
           (== zero [a b c e f g])


           (everyg #(membero % one) ["a" "b"])
           (everyg #(membero % four) ["a" "b" "c" "d"])
           (everyg #(membero % seven) ["a" "b" "f"])
           (everyg #(membero % eight) ["a" "b" "c" "d" "e" "f" "g"])

           ))
       first
       (map set))
      parse-map {zero 0 one 1 two 2 three 3 four 4 five 5 six 6 seven 7 eight 8 nine 9}]
  (map parse-map digits))
