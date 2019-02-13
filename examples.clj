(map inc [1 4 5]) 

(reduce + 0 [2 4 5 6])

(apply  + 0 [2 4 5 6])

(range  1 5)


(defn factorial "factorial: (apply * (range 1 n))" [n]  (apply * (range 1 n)))

(factorial 0)

(take 5 (repeat "hello"))

(take 10 (drop 5 (iterate dec 10)))

(def lst (interleave [1 2 3] ["cat" "dogs" "pigs"]))

lst 

(partition 3 lst)

(zipmap [:key1 :key2] ["value one" "value two"])

(let [s "I am a string"   
      lst [2 3]      
      [e1 e2] lst]    
      (str "s = " s ", lst = " lst ", e1 = " e1 ", e2 = "e2))

(["meow" "woof"] 1)

({:cat "meow" :dog "woof"} :dog)

(#{"cat" "dog"} "cat")

(#{"cat" "dog" } nil)

(def numbers ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"])
(def tens09  (zipmap numbers (range 0 10)))

(def smaller3 (partial > 3))
(smaller3 1)
(smaller3 4)

(split-with smaller3 [0 1 2 3 4 5])
