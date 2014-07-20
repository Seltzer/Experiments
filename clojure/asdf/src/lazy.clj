(ns lazy)

;; The comments below should be read in the numbered order
;; to better understand this code.
 
;(defn big-computation [x] (* x x))
(defn big-computation [x] (Thread/sleep 1000) (* 10 x))

(def nums (range))


(time                              ; [7] time the transaction
  (def v                           ; [6] save vector as v
    (apply vector                  ; [5] turn the list into a vector
           (map big-computation    ; [4] process each item for 1 second
                (take 5            ; [3] take first 5 from filtered items
                      (filter      ; [2] filter items 10000 to 10010
                        (fn [x] (and (> x 10000) (< x 10010)))
                        nums)))))) ; [1] nums = 1 billion items



(println v)


