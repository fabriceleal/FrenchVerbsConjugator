(ns conjugator.main)

(refer 'conjugator.grammar-rules)
(refer 'conjugator.irregulars)
(refer 'conjugator.rulesengine)

;; Rules for full conjugation of verbs
;; * If verb is irregular, dispatch to conjugator/irregulars
;;
;; * If verb is not irregular, it must dispatch to 
;; * the default, automatic conjugator

(def *verbs* `(
     ;; Irregular verbs dispatcher
     (~(fn[verb, tense] 
         (is-irregular verb tense)) 
       ~(fn[verb, tense] 
          (get-irregular-conjugation verb tense)))
     
     ;; Default conjugator
     (~(fn[verb, tense] 
         true)  
       ~(fn[verb, tense] 
          (regular-rule verb tense)))))

;; ---------------

;; Function to call from the exterior world

(defn conjugate-verb [verb tense]
	(evaluate-rules-scalar (list verb tense) *verbs*))
	
;; ---------------