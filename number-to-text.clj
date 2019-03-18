(ns number-to-text
  (:require [clojure.pprint :as pp]
            [clojure.string :as s]))

; Constants 
(def unit-million  1000000)
(def unit-thousand  1000)
(def unit-hundred   100)
(def unit-one       1)

; Map of text to single digit numbers: {"zero" [0 :digit] "one" [1 :digit] ...}
(def digits ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def tens09 (zipmap  (range 0 10) digits))

; Map of text to teens double-digits
(def teens ["ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"])
(def teens1019 (zipmap (range 10 20) teens))

; Map of text to 20,30,...,90 double-digits
(def tees ["twenty" "thirty"  "forty" "fifty" "sixty" "seventy" "eighty" "ninety"])
(def tees2090 (zipmap (range 20 100 10) tees))

; Map of text to units of hundreds, thousands, millions
(def units ["hundred" "thousand" "million" ])
(def units-digits [100 1000 1000000])
(def units-map (zipmap units-digits units))

; Mapping of numbers to words
(def num->word (conj tens09 teens1019 tees2090 units-map))

; Forward declarations
(declare num->text+unit three-digit->text two-digit->text)

; Convert a positive number between 0 and 999 millions into its textual form
; usage: (num-text 123) -> "one hundred twenty three"
(defn number->text [n]
    (s/trim
        (let [millions (/ n one-million) 
                thousands (/ (- n millions) one-thousand)) 
                hundreds  (mod n one-thousand)]
                (s/str  (num->text+unit    millions  unit-million  three-digit->text)  
                        (num->text+unit    thousands unit-thousand three-digit->text)  
                        (three-digit->text hundreds))))


;; Format a positive number into text and combined with its unit in text form
;; e.g. (num->text+unit 123 1000 three-digit->text) -> one hundred twenty three thousand 
(defn num->text+unit [num unit n->t]
    (if (> num 0) 
        (s/str (n->t num) " " (num->word unit) " ")
        ""))


;; Convert a 3-digit number (0 to 999) to text
(defn three-digit->text [num]
    (let [hundred        (/ num 100) 
          hundred-text   (num->text+unit hundred unit-hundred num->word)
                         ;; (if (> hundred 0) (s/str (num->word hundred) " " (num->word 100) " ")  "")]
        (s/str hundred-text (two-digit->text (mod num 100)))))


; Convert a 2-digit number between 0 and 99 to text
(defn two-digit->text [n]
    (if-let [text (num->word n)] 
        text
        (let [single (n % 10) 
              tens   (n - single)]
            (s/str (num->word tens) " " (num->word single)))))
