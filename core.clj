(ns nlp-calculator
  (:require [clojure.pprint :as pp]
            [clojure.string :as s]))

;
; Maps for parsing commands and arithmetic operations: + - * /
;
(def commands {"compute" identity "equals" identity})


(def decimal-point "point")

; Map of text to single digits: {"zero" [0 :digit] "one" [1 :digit] ...}
(def numbers ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def tens09  (zipmap numbers (partition 2 (interleave  (range 0 10) (repeat 10 :digit )))))


; Map of text to teens double-digits
(def teens ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])
(def teens1019 (zipmap teens (partition 2 (interleave  (range 10 20) (repeat 10 :teen )))))


; Map of text to 20,30,...,90 double-digits
(def tees ["twenty" "thirty"  "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])
(def tees2090 (zipmap tees (partition 2 (interleave  (range 20 100 10) (repeat 10 :tea )))))


; Map of text to 100
(def hundreds ["hundred" "hundreds"])
(def hundred-digits [100 100])
(def hundreds100 (zipmap hundreds (partition 2 (interleave  hundred-digits (repeat (count hundreds) :hundred )))))


; Map of text to thousands of 10 from 1000 to one million
(def thousands ["thousand" "thousands" "million" "millions"])
(def thousand-digits [1000 1000 1000000 1000000])
(def thousands-up (zipmap thousands (partition 2 (interleave  thousand-digits (repeat (count thousands) :thousands)))))


; Combined map for the conversion of words to numbers
(def number-dict (conj tens09 teens1019 tees2090 hundreds100 thousands-up))


; Function names used to define the state-manchine
(declare times10plus times100plus times plus next-subtotal)


;
; State Machine for conversion words to numbers
;
; It is a map whose keys are vectors and values are functions. The keys are keyword-pairs
; of the types of the two input numbers e.g. 0-9, or 10-11, or 20,30,..90, etc.
; The values of the map are functions to apply to the two input numbers according to their types
;
(def stm {[:digit :digit]    times10plus      [:digit :teen]   times100plus
          [:digit :tea]  times100plus         [:digit :hundred] times
          [:digit :thousands] times

          [:teen :digit]   times10plus        [:teen :teen]    times100plus
          [:teen :tea] times100plus           [:teen :hundred]  times
          [:teen :thousands]  times

          [:tea :digit]    plus               [:tea :teen]   times100plus
          [:tea :tea]  times100plus           [:tea :hundred] times
          [:tea :thousands] times

          [:hundred :digit]    plus           [:hundred :teen]   plus
          [:hundred :tea]      plus           [:hundred :hundred] times
          [:hundred :thousands] times

          [:thousands :digit] next-subtotal    [:thousands :teen]   next-subtotal
          [:thousands :tea]   next-subtotal    [:thousands :hundred] next-subtotal
          [:thousands :thousands]  times
        })

(defn times10plus [[acc acc-type] [number number-type] subtotals]
  (let [value  (+ (* acc 10) number)
        res-type (if (> acc number) acc-type number-type)
        result [value res-type]]
    [result subtotals]))


(defn times100plus [[acc acc-type] [number number-type] subtotals]
  (let [value  (+ (* acc 100) number)
        res-type (if (> acc number) acc-type number-type)
        result [value res-type]]
    [result subtotals]))


(defn times [[acc acc-type] [number number-type] subtotals]
  (let [product  (* acc number)
        res-type (if (> acc number) acc-type number-type)
        result   [product res-type]]
    [result subtotals]))


(defn plus [[acc acc-type] [number number-type] subtotals]
  (let [sum      (+ acc number)
        res-type (if (> acc number) acc-type number-type)
        result   [sum res-type]]
    [result subtotals]))


(defn next-subtotal [[acc acc-type] [number number-type] subtotals]
  (let [new-subtotals (conj subtotals acc)
        result         [number number-type]]
    [result new-subtotals]))


; Convert a list of words into a number using map: number-dict, and state-machine: stm
(defn to-digits [word-list]
  (println word-list (map number-dict word-list))
  (let [number-list (map number-dict word-list)
        [[last-value last-type] subtotals]
              (reduce  (fn [acc new-number]
                          (let [[acc-number subtotals]   acc
                                [acc-value acc-type]     acc-number
                                [new-val new-type]       new-number
                                f (stm [acc-type new-type])]
                            (f acc-number new-number subtotals)))

                        [[0 :digit] []]
                        number-list)]

    (apply + last-value  subtotals)))



(def op-words ["plus" "minus" "multiply" "times" "divide"])
(def op-fns [+ - * * /])

(def op-map (zipmap op-words op-fns)) ; a mapping of operator-text to corresponding math functions
(def op-set (set op-words))           ; a set of operator words


; Split "words" into [operand operator rest-of-words]
(defn split-ops [[_ _ words]]
  (let [[operand rest-of-words] (split-with (complement op-set) words)]
    [operand (first rest-of-words) (rest rest-of-words)]))


; Translate "words" into a list of aritmetic operations
; e.g. ["two" "plus" "three" "minus" "four"] -> [2 + 3 - 4]
(defn to-calculations [words]
  (drop-last
            (reduce (fn [acc [operand operator _]]
                      (concat acc [(to-digits operand)] [(op-map operator)]))
                    []
                    (take-while (fn [[operand _ _]] (not (empty? operand)))
                                (iterate split-ops [["zero"] "plus" words])))))


; Compute the result of the list of arithmetic operations
;
; when op is a number, acc is a partial function to be applied to op to carry out the current arithmetic operation and
; return the value of the computation.
; Otherwise, op is an arithmetic operator, and acc is an operand, return a partial function to apply to
; the next operand
(defn compute [calculations]
  (let [result (reduce (fn [acc op] (if (number? op) (acc op) (partial op acc)))
                       (partial + 0)
                       calculations)]

    (if (ratio? result) (double result) result)))



; Words not needed in the conversion of text to number
(def unused-words #{"by" "and"})


; Evaluate a series of arithmetic operations in natural language and return the result as a number
(defn evaluate [text]
  (as-> text % (s/trim %) (s/split % #" +") (filter (complement unused-words) %)
        (to-calculations %) (compute %))


(def test-data " two thousand five hundred twenty one multiply three hundreds forty three thousands five hundreds thirty three  ")


; three hundreds forty three thousands five hundreds thirty three
; -> 343533
(* 343533 2521)

; two thousands five hundred twenty one
; ) -> 2521
