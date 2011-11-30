(ns conjugator.rulesengine)

;; The rules engine goes *here*

;; Rules list:
;; * in the form:((predicate_1(...)_as_bool function_1(...)_as_obj)
;;               (predicate_2(...)_as_bool function_2(...)_as_obj)
;;               ....
;;               (predicate_n(...)_as_bool function_n(...)_as_obj)
;; ---------------
;;(def *teste* `(
;;    (~(fn [x] false) ~(fn [x] (+ x 1)))
;;    (~(fn [x] false) ~(fn [x] (+ x 2)))
;;    (~(fn [x] true) ~(fn [x] (+ x 3)))
;;    (~(fn [x] false) ~(fn [x] (+ x 4)))))

(defn rule-list-predicate   [rule] (first rule))
(defn rule-list-transformer [rule] (first (rest rule)))

;; Returns the 1st valid pair predicate / rule

(defn rule-for-item-list [par-predicate rules] 
  (first 
    (filter 
      (fn [x] (apply (rule-list-predicate x) par-predicate)) 
      rules)))

(defn locate-and-evaluate [par-transf par-pred rules]
  (apply 
    (rule-list-transformer (rule-for-item-list par-pred rules)) 
    par-transf))

(defn evaluate-rules [input-list rules]
  (map 
    (fn[x] (locate-and-evaluate x x rules))
    input-list))

(defn evaluate-rules-scalar [input rules]  
  (first (evaluate-rules (list input) rules)))

