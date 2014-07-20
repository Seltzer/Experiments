(ns bodyparts)

(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

(defn has-matching-part?
  [part]
  (re-find #"^left-" (:name part)))

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)})


(defn my-reduce
  ([f initial coll]
     (loop [result initial
            remaining coll]
       (let [[current & rest] remaining]
         (if (empty? remaining)
           result
           (recur (f result current) rest)))))
  ([f [head & tail]]
     (my-reduce f (f head (first tail)) (rest tail))))


(defn symmetrize-body-parts
  "Expects a seq of maps which have a :name and :size"
  [asym-body-parts] ; 
  (loop [remaining-asym-parts asym-body-parts ; ~~~2~~~
         final-body-parts []]
    (if (empty? remaining-asym-parts) ; ~~~3~~~
      final-body-parts
      (let [[part & remaining] remaining-asym-parts ; ~~~4~~~
            final-body-parts (conj final-body-parts part)]
        (if (has-matching-part? part) ; ~~~5~~~
          (recur remaining (conj final-body-parts (matching-part part))) ; ~~~6~~~
          (recur remaining final-body-parts))))))

(println "\n\n\n")
(println (reduce + 1 [1 2 3 4]))


(defn better-symmetrize-body-parts
  "Expects a seq of maps which have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (let [final-body-parts (conj final-body-parts part)]
              (if (has-matching-part? part)
                (conj final-body-parts (matching-part part))
                final-body-parts)))
          []
          asym-body-parts))


(println "\n\n\n\n")
(println asym-hobbit-body-parts)

(better-symmetrize-body-parts asym-hobbit-body-parts)

(println "\n\n\n\n")
(println (symmetrize-body-parts asym-hobbit-body-parts))

(println (has-matching-part?))

(defn hit
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
        body-part-size-sum (reduce + 0 (map :size sym-parts))
        target (inc (rand body-part-size-sum))]
    (loop [[part & rest] sym-parts
           accumulated-size (:size part)]
      (if (> accumulated-size target)
        part
        (recur rest (+ accumulated-size (:size part)))))))

(hit asym-hobbit-body-parts)
; => {:name "right-upper-arm", :size 3}

(hit asym-hobbit-body-parts)
; => {:name "chest", :size 10}

(hit asym-hobbit-body-parts)
; => {:name "left-eye", :size 1}
