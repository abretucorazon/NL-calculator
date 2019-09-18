;;------------------------------------------------------
;; Basic Repl usage
;;------------------------------------------------------
(require '[clojure.string :as str])
(str/upper-case "clojure")

(require '[clojure.string :refer [upper-case]])
(upper-case "clojure")

;; documentation
(doc clojure.string/upper-case)

(find-doc "indexed")

(apropos "macro")

(source some?)

(dir clojure.string)

;; Java doc
(require '[clojure.java.javadoc :as jdoc])
(jdoc/javadoc java.io.InputStream)

;;------------------------------------------------------
;; Print format
;;------------------------------------------------------
(require '[clojure.pprint :as pp])

(defn num-prop [x]
    { :n x
      :even? (even? x)
      :doubled (+ x x)
      :squared (* x x)
      :pos? (< 0 x)
      :digits (count (str x))})

(def result (mapv num-prop (range 8 15)))

(pp/pprint result)

(pp/print-table result)

;; number of nested levels
(def x [ 1 [ 2 [3 [4]]]]) 
(set! *print-level* 3)   ;; number of nested levels
x
(set! *print-level* nil) ;; unlimited nested levels
x

;; print first n elements of collection
(def y [1 2 3 4 5 6])
(set! *print-length* 3)   ;; print first 3 elements of collection
y
(set! *print-length* nil) ;; print all elements of collection
y

;; print last exception
(pst *e) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; REPL DEBUGGING TIPS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;------------------------------------------------------
;; def global binding  
;;------------------------------------------------------
(defn foo
      [n]
      (cond (> n 40) (+ n 20)
            (> n 20) (- (first n) 20)
            :else 0))

(foo 24)

(def n 24)


;;------------------------------------------------------
;; prn
;;------------------------------------------------------
(defn average
  "a buggy function for computing the average of some numbers."
  [numbers]
  (let [sum (first numbers)
        n (count numbers)]
    (prn sum) ;; HERE printing an intermediary value
    (/ sum n)))

(average [ 20 34 9])


;;------------------------------------------------------
;; doto & prn
;;------------------------------------------------------
(defn average
  "a buggy function for computing the average of some numbers."
  [numbers]
  (let [sum (first numbers)
        n (count numbers)]
    (/
      (doto sum prn) ;; HERE
      n)))

(average [ 20 34 9])


;;------------------------------------------------------
;; Let binding & prn 
;;------------------------------------------------------
(defn average
  "a buggy function for computing the average of some numbers."
  [numbers]
  (let [sum (first numbers) 
        _ (prn "sum: " sum) ;; prn
        n (count numbers)
        _ (prn "n: " n)     ;; prn
        ]
    (/ sum n)))

(average [ 20 34 9])


;;------------------------------------------------------
;; dlet macro  
;;------------------------------------------------------
(defmacro dlet
  "let with inspected bindings"
  [bindings & body]
  `(let [~@(mapcat (fn [[n v]]
                     (if (or (vector? n) (map? n))
                       [n v]
                       [n v '_ `(println (name '~n) ":" ~v)]))
                   (partition 2 bindings))]
     ~@body))


;;------------------------------------------------------
;; dlet binding  
;;------------------------------------------------------
(defn average
  "a buggy function for computing the average of some numbers."
  [numbers]
  (dlet [sum (first numbers)        
        n (count numbers)]
    (/ sum n)))

(average [ 20 34 9])    



;;------------------------------------------------------
;; def spyscope library
;;------------------------------------------------------
(require 'spyscope.core :as spy)
 