(ns nlp-calculator
  (:require [clojure.pprint :as pp]
            [clojure.string :as s]))


; Map of text to single digit numbers: {"zero" [0 :digit] "one" [1 :digit] ...}
(def numbers ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def tens09  (zipmap numbers (range 0 10)))


; Map of text to teens double-digits
(def teens ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])
(def teens1019 (zipmap teens (range 10 20)))


; Map of text to 20,30,...,90 double-digits
(def tees ["twenty" "thirty"  "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])
(def tees2090 (zipmap tees (range 20 100 10)))


; Map of text to hundreds
(def hundreds ["hundred" "hundreds"])
(def hundreds-digits [100 100])
(def hundreds-map (zipmap hundreds hundreds-digits))

; Map of text thousands, millions
(def thousands ["thousand" "thousands" "million" "millions"])
(def thousands-digits [1000 1000 1000000 1000000])
(def thousand-map (zipmap thousands thousands-digits))
(def thousands-set (set thousands-digits))
(defn thousands? [n] (thousands-set n)) ; test if n is a thousand or a million .. 
(defn not-thousands? [n] (not (thousands? n))) ; test if n is not a thousand nor a million ...

; Combine the above 2 vectors of hundreds and thousands into 
; a map of text to hundred, thousand, million
(def powers (concat hundreds thousands))
(def powers-digits (concat hundreds-digits thousands-digits))
(def hundreds-up (zipmap powers powers-digits))

(def powers-set (set powers-digits))
(defn is-power [n] (powers-set n)) ; Test if number is one of the powers: 100, 1000, 1000000

; Combined map for the conversion of words to numbers
(def number-dict (conj tens09 teens1019 tees2090 hundreds-up))

;(def t1 " two thousand five hundred twenty one")

; Convert a list of numbers into its corresponding number between 0 and 999
; e.g. "three hundreds forty five" => [3 100 40 5] => 345
(defn to-hundreds [number-list]
  (let [[mult-list rem-list] (split-with (partial > 100) number-list)
        mult (apply + mult-list)        ; multiplier of 100
        rem  (apply + (rest rem-list))] ; modulus of 100
    (if-not (empty? rem-list)           ; number >= 100
      (+ (* mult 100) rem)              ; number = multiplier * 100 + modulus
      mult)                             ; number < 100
  ))

; Convert a list of numbers literally translated from words, into the number the original text denotes
; the resulting number is assumed to be: 0 <= n <= the largest number found in vector 'thousands-digits'
; e.g. "two hundred thousands five hundred twenty one" => [2 100 1000 5 100 20 1] => 200521
(defn to-number [number-list]
  (loop [acc 0 xs number-list]
    (println "acc:" acc "\nnumber-list:" xs)
    (let [[mult-list rem-list] (split-with not-thousands? xs)
          mult (to-hundreds mult-list)] ; multiplier of thousand-power is a 3-digit number 
      (println "mult-list:" mult-list  "\nrem-list:" rem-list "\n")
      (if (empty? rem-list)            ; if    reached the end of 'number-list'
        (+ acc mult)                   ; then  return result
        (-> mult                       ; else  a thousand-power is found eg. 'thousand' or 'million'
            (* (first rem-list))       ; multiplier * thousand-power
            (+ acc)                    ; acc + (multiplier * thousand-power)
            (recur (rest rem-list)))   ; recurse for the rest of numbers after thousand-power
      ))))


      

; Convert a list of words into a number using map: number-dict
(defn to-digits [word-list]
(println word-list (map number-dict word-list))
  (->> (map number-dict word-list)
       (reduce  (fn [acc number]
                  (println acc number)
                  (if-not (is-power number)
                    (conj acc number)
                    (let [[smaller larger] (split-with (partial > number) acc)
                          coef (apply + smaller)]
                      (conj larger (* coef number)))))
                '())
       (apply +))
)


(def op-words ["plus" "minus" "multiply" "times" "divide"])
(def op-fns [+ - * * /])

(def op-map (zipmap op-words op-fns)) ; a mapping of operator-text to corresponding math functions
(def op-set (set op-words))           ; a set of operator words
(defn is-operator [w] (op-set w))     ; test is a word denotes an arithmetic operator: "plus", "minus",...

; Translate "words" into a list of aritmetic operations
; e.g. ["two" "plus" "three" "minus" "four"] -> [2 + 3 - 4]
(defn to-calculations [words]
  (loop [lst words acc []]
    (let [[num-text rest-words] (split-with (partial (complement op-set)) lst)]
(println num-text rest-words acc)
      (if-not (empty? rest-words)
        (recur (rest rest-words) (conj acc (to-digits num-text) (op-map (first rest-words)))) 
        (conj acc (to-digits num-text))
        )))
)


; Compute the result of the list of arithmetic operations
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

; Dictionary of all the words recognized by this program
(def vocabulary (conj  number-dict op-map (zipmap unused-words unused-words)))
(defn validate [words]
  "Return a list with typing error(s) or 'words' if there is no error"
    (let [errors (filter (fn [w] (nil? (vocabulary w))) words)]
      (if (empty? errors)
        words
        (do (println "Typing error(s): " errors) 
            []))))


(defn evaluate [text]
  "Evaluate a series of arithmetic operations in natural language and return the result as a number"
  (as-> text % (s/trim %) (s/split % #" +") (filter (complement unused-words) %) 
        (validate %) (to-calculations %) (compute %)))

(def test-data " two thousand five hundred twenty one multiply three hundreds forty three thousands five hundreds thirty three  ")

;(def t1 " two thousand five hundred twenty one")
(def t2 "three hundreds forty three thousands five hundreds thirty three")
(evaluate t1)
(evaluate t2)
(evaluate test-data)
; three hundreds forty three thousands five hundreds thirty three
; -> 343533
(* 343533 2521)