(ns asdf.core)


(defn square [x]
  (* x x))

(println (println (square 3)))




(println (+ 1 2))




(println '(square 3))
(println (square 3))


(println (str "abcd" "efgh" 'i))



(println "asdf")
(println :asdf)
(println (keyword "asdf"))


(println '("a" "b" "c"))

(println (list "a" "b" "c"))

(println #{ "d" "e" "f"})


(println { "a" "b" "c" "d" })
(println (hash-map "a" 1))


(set '(1 1 1 1 1 2 3 4))
(println (set '(1 1 1 1 1 2 3 4)))


(def vowels #{ "a" "e" "i" "o" "u" })


(defn pig-latin [word] ; defines a function
  ; word is expected to be a string
  ; which can be treated like a sequence of characters.
  (let [first-letter (first word)] ; assigns a local binding
    (if (contains? vowels first-letter)
      (str word "ay") ; then part of if
      (str (subs word 1) first-letter "ay")))) ; else part of if

(println (pig-latin "red"))
(println (pig-latin "orange"))

(println (vowels "a"))
(println (vowels "b"))


