(ns number-to-text
  (:require [clojure.pprint :as pp]
            [clojure.string :as s]))

; Constants
(def unit-million  1000000)
(def unit-thousand  1000)
(def unit-hundred   100)
(def unit-ten       10)

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
(def units ["hundred" "thousand" "million"])
(def units-digits [100 1000 1000000])
(def units-map (zipmap units-digits units))

; Mapping of numbers to words
(def num->word (conj tens09 teens1019 tees2090 units-map))

; Forward declarations
(declare num->text+unit three-digit->text two-digit->text n->3digits)

(def no-text "")
(def ordered-units [(num->word unit-million) (num->word unit-thousand) no-text])

; Ranges for validating input
(def valid-min 0)
(def valid-max 999999999)
(def bad-input-msg (str "Number must be between " valid-min " and " valid-max))

(defn one-digit?   [n] (and (>= n 0) (<= n 9)))
(defn two-digit?   [n] (and (>= n 10) (<= n 99)))
(defn three-digit? [n] (and (>= 100) (<= n 999)))
(defn four-digit?  [n] (and (>= n 1000) (<= n 9999)))

(defn n->t [n]
    (assert (and (>= n valid-min) (<= n valid-max)) bad-input-msg)
    (cond 
      (one-digit?   n)    (num->word n)
      (two-digit?   n)    (two-digit->text n)
      (three-digit? n)    (three-digit->text n)
      :else 
            (let [lst-3digits (n->3digits n) 
                  vec-units   (subvec ordered-units (- (count ordered-units) (count lst-3digits)))
                  vec-text    (map (fn [n u] (if (zero? n) no-text (str (three-digit->text n) u " "))) 
                                   lst-3digits vec-units)]
         ; (println lst-3digits vec-units vec-text)                      
                (s/trim (apply str vec-text)))))

; Split a number into a list of 3-digit 
(defn n->3digits [n]
    (loop [x n res '()]
        (if (= x  0)
            res 
            (recur (quot x 1000) (conj res (mod x 1000))))))


;; Convert a 3-digit number (0 to 999) to text˜
(defn three-digit->text [num]
    (let [hundred        (quot num unit-hundred) 
          tens           (mod num unit-hundred)
          hundred-text   (if (zero? hundred) no-text (str (num->word hundred) " " (num->word unit-hundred) " "))
          tens-text      (if (zero? tens) no-text (str (two-digit->text tens) " "))]
;(println hundred hundred-text tens tens-text)
        (str hundred-text tens-text)))


; Convert a 2-digit number between 0 and 99 to text
(defn two-digit->text [n]
    (if-let [text (num->word n)] 
        text
        (let [single (mod n 10) 
              tens   (- n single)]
            (str (num->word tens) " " (num->word single)))))
