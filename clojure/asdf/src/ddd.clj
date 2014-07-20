(ns ddd)


(defstruct employee :name :id)
(struct employee "Mr. X" 10)               
(struct-map employee :id 20 :name "Mr. Y") 
 
(def a (struct-map employee :id 20 :name "Mr. Y"))
(def b (struct employee "Mr. X" 10))'


(println "A")
(println (:id a))
(println (a :id))
(println (:name a))
(println (a :name ))

(println "\n\nB")
(println(:id b))
(println(b :id))
(println(b :name))
(println(:name b ))


(println "\n\n\n\n")
(println b)
 
 
(def b1 (assoc b :function "engineer"))
(println b1)
  
 
(def b2 (dissoc b1 :function)) ; this works as :function is instance
(println b2)



(def b3 (assoc b :function "artiste"))
(println b3)

(try (/ 1 0) (catch Exception e (prn "in catch")) (finally (prn "in finally")))
