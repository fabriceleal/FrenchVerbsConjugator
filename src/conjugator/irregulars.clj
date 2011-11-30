(ns conjugator.irregulars)

(refer 'conjugator.grammar-rules)
(refer 'conjugator.rulesengine)

;; Should allow for literal and lazy indexing of irregular
;; Must allow for both literal and lazy conjugation of irregulars

(def *irregulars-fr* {
    '(("aller") (:present-tense))     
    (fn[verb tense persons] '(
         (:je "vais") (:tu "vas") (:il "va") (:elle "va") (:on "va")
         (:nous "allons") (:vous "allez") (:ils "vont") (:elles "vont")))
    
    '(("avoir") (:present-tense)) 
    (fn[verb tense persons] '(
         (:je "ai") (:tu "as") (:il "a") (:elle "a") (:on "a")
         (:nous "avons") (:vous "avez") (:ils "ont") (:elles "ont")))
    
    '(("battre") (:present-tense)) 
    (fn[verb tense persons] '(
         (:je "bats") (:tu "bats") (:il "bat") (:elle "bat") (:on "bat")
         (:nous "battons") (:vous "battez") (:ils "battent") (:elles "battent")))
    
    '(("boire") (:present-tense)) 
    (fn[verb tense persons]'(
         (:je "bois") (:tu "bois") (:il "boit") (:elle "boit") (:on "boit")
         (:nous "buvons") (:vous "buvez") (:ils "boivent") (:elles "boivent")))
    
    '(("conduire" "construire" "détruire" "produire" "traduire") 
       (:present-tense)) 
    (fn[verb tense persons] '(
       ;; Take off -re (2)
       ;; Use terminations: -s, -s, -t, -sons, -sez, -sent

       (let [len (.length verb)
             steam (.substring verb 0 (- len 2))]
         (persons-conjugator 
           steam 
           {:s-st "s" :s-nd "s" :s-rd "t" 
            :p-st "sons" :p-nd "sez" :p-rd "sent" } 
           persons 
           nil))))
    
    '(("connaître" "apparaître" "disparaître" "paraître" "reconnaître") 
       (:present-tense)) 
    (fn[verb tense persons] '(
      ;; Take off -ître (4)
      ;; Use terminations: -is, -is, -ît, -issons, -issez, -issent
      (let [len (.length verb)
             steam (.substring verb 0 (- len 4))]
         (persons-conjugator 
           steam 
           {:s-st "is" :s-nd "is" :s-rd "ît" 
            :p-st "issons" :p-nd "issez" :p-rd "issent" } 
           persons 
           nil))))
    
    '(("courir") (:present-tense)) 
    (fn[verb tense persons] '(
         (:je "cours") (:tu "cours") (:il "court") (:elle "court") (:on "court")
         (:nous "courons") (:vous "courez") (:ils "courent") (:elles "courent")))
    
    '(("craindre" "contraindre" "éteindre" "feindre" "peindre" "plaindre"
       "rejoindre") (:present-tense)) 
    (fn[verb tense persons] '(
      ;; Take off -ndre (4)
      ;; Use terminations: -ns, -ns, -nt, -gnons, -gnez, -gnent
      (let [len (.length verb)
             steam (.substring verb 0 (- len 4))]
         (persons-conjugator 
           steam 
           {:s-st "ns" :s-nd "ns" :s-rd "nt" 
            :p-st "gnons" :p-nd "gnez" :p-rd "gnent" } 
           persons 
           nil))))
    
    '(("croire") (:present-tense)) 
    (fn[verb tense persons] '(
         (:je "crois") (:tu "crois") (:il "croit") (:elle "croit") (:on "croit")
         (:nous "croyons") (:vous "croyez") (:ils "croient") (:elles "croient")))
    
    '(("devoir") (:present-tense)) 
    (fn[verb tense persons] '(
         (:je "dois") (:tu "dois") (:il "doit") (:elle "doit") (:on "doit")
         (:nous "devons") (:vous "devez") (:ils "doivent") (:elles "doivent")))
    
    '(("dire") (:present-tense)) 
    (fn[verb tense persons] '(
         (:je "dis") (:tu "dis") (:il "dit") (:elle "dit") (:on "dit")
         (:nous "disont") (:vous "dites") (:ils "disent") (:elles "disent")))
    })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This shouldn't be done here ... :(
(def *irregulars* *irregulars-fr*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: ....
(defmacro irregular-hardcoded [full-list persons]
  ())

;; TODO: ...
(defmacro irregular-with-terminations [verb ending-length terminations persons]
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn get-with-key [verb tense]
  (get *irregulars* (list (list verb) (list tense)) nil))

;; Just check if pair verb-tense is indexed
(defn is-irregular[verb tense] 
  (not 
    (nil? (get-with-key verb tense))))

;; Get pair key-value(lambda)
;; return the result of the passage of [verb tense persons] to the lambda found
(defn get-irregular-conjugation[verb tense] 
  ((get-with-key verb tense)
    verb 
    tense 
    (tense-to-persons tense)))
