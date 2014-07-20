(ns lazy2)



(defn square [x]
  (* x x))

(def inf (iterate square 2))

(println (take 6 inf))





(def nums (iterate inc 0))
(def s (for [x nums :when (zero? (rem x 4))] (inc x)))
(println(take 5 s))



;(def v (apply vector (take 4 (iterate #(% * %) 2))))


;(println v)