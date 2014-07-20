(ns maps
  
  
  )


;(require asdf.core)


             

(def rofls [ "aaa" "bbb" "ccc"])


(println
  (str 
    {:a 1 :b 2}
    (hash-map :a 1 :b 2)
    
    (:a {:a 1 :b 2 :c 3})
    (get {:a 1 :b 2 :c 3} :a)
    (:d {:a 1 :b 2 :c 3} "ASDFASDF")
    (get {:a 1 :b 2 :c 3} :a)
    (get {:a 1 :b 2 :c 3} :d "ASDFASDF")  
    
    (do (get {:a 1 :b 2 :c 3} :a) (get {:a 1 :b 2 :c 3} :a))
    
    
    'rofls
    rofls
    (eval  'rofls)
    
    ;(asdf.core\square 3)
  ))




;'failed-protagonist-names
; => failed-protagonist-names

;(eval 'failed-protagonist-names)
; => ["Larry Potter" "Doreen the Explorer" "The Incredible Bulk"]