(ns conjugator.grammar-rules)

(refer 'conjugator.rulesengine)

(def *persons-none* '(
		:none))

(def *persons-third* '(
		:s-nd
		:p-nd))

(def *persons-full* '(
		:s-st :s-nd :s-rd 
		:p-st :p-nd :p-rd))

(def *persons-resolved* {
		:none '(:NA)
		:s-st '(:je) 
		:s-nd '(:tu) 
		:s-rd '(:il :elle :on) 
		:p-st '(:nous) 
		:p-nd '(:vous) 
		:p-rd '(:ills :elles)})

(def *moods* '(
		:indicative
		:subjunctive
		:imperative
		:conditional
		:infinitive
		:participle))
		
(def *tenses* '(
		:present-tense 
		:past-composed 
		:past-imperfect 
		:past-simple 
		:past-pluperfect 
		:future 
		:gerund
		:passive-voice
		:imperative
		:subjunctive
		:conditional
		:infinitive
		:past-participle
		:present-participle))

(def *tenses-to-moods* {
		:present-tense 		  :indicative
		:past-composed 		  :indicative
		:past-imperfect 	  :indicative
		:past-simple 		    :indicative
		:past-pluperfect 	  :indicative
		:future 			      :indicative
		:gerund				      :indicative
		:passive-voice		  :indicative
		:imperative			    :imperative
		:subjunctive		    :subjunctive
		:conditional		    :conditional
		:infinitive			    :infinitive
		:past-participle	  :participle
		:present-participle :participle
		})
		
(def *mood-to-persons* {
		:indicative  *persons-full*
		:subjunctive *persons-full*
		:imperative  *persons-full*
		:conditional *persons-full*
		:infinitive  *persons-none*
		:participle  *persons-none*})

;; ---------------

;; Returns a list in the form ((je s-st) (tu s-nd) (il s-rd) (elle s-rd) ...)

(defn tense-to-persons-person [list-item] (first list-item))
(defn tense-to-persons-type   [list-item] (second list-item))

(defn tense-to-persons [tense]
	(mapcat 
		(fn [x] 
			(map (fn [y] (list y x)) (get *persons-resolved* x))) 
		(get *mood-to-persons* 
			(get *tenses-to-moods* tense))))

;; ---------------

;; Returns a list with the steam, the ending of the verb, and the full verb

(defn verb-parts-extract-steam 	[parts] (first parts))
(defn verb-parts-extract-ending [parts] (second parts))
(defn verb-parts-extract-verb   [parts] (second (rest parts)))

(defn verb-parts-extract [verb]
  (let 
    [len (.length verb)]    
    (list (.substring verb 0 (- len 2)) 
    (.substring verb (- len 2) len) 
    verb)))

;; ---------------

;; Build here a basic concatenator

(defn persons-conjugator [
       ;; Used in "regular cases", in concatenation
       steam 
       ;; Used in "regular cases", in concatenation
       ;; {person "termination" person "termination" ... }
       map-terminations
       ;; ((:je :s-st ) (:tu :s-nd) ... )
       list-persons
       ;; Used as exceptions, return here the *full verb*
       ;; {person "full-verb" person "full-verb" ... }
       map-exceptions 
       ] 
  ;; Search list of exceptions (return if match)
  ;; Generate using concatenation with the termination (return)
  (map 
     (fn[x] 
       (let 
         [person (tense-to-persons-person x)
          value-if-exception (get map-exceptions person nil)
          value-if-regular (if (nil? value-if-exception) (.concat steam (get map-terminations person "")) nil)
          conjugated (if (nil? value-if-exception) value-if-regular value-if-exception)]
         (list person conjugated)))
     list-persons))

;; PRESENT *********************************************************

(def *endings-present-fr-er* {
   :s-st "e" :s-nd "es" :s-rd "e"
   :p-st "ons" :p-nd "ez" :p-rd "ent"})

(def *endings-present-fr-ir* {
   :s-st "is" :s-nd "is" :s-rd "it"
   :p-st "issons" :p-nd "issez" :p-rd "issent"})

(def *endings-present-fr-re* {
   :s-st "s" :s-nd "s" :s-rd ""
   :p-st "ons" :p-nd "ez" :p-rd "ent"})

(def *endings-present* {
   "er" *endings-present-fr-er*
   "ir" *endings-present-fr-ir*
   "re" *endings-present-fr-re*})


(defn get-ending-present [termination person-type]
  (get (get *endings-present* termination) person-type))

(defn endings-resolved [endings-by-person-type]
  (apply sorted-map 
         (mapcat
           (fn[outer-item] 
             (let [outer-key (first outer-item)
                   termination (second outer-item)]
             (mapcat
              (fn[inner-item] (list inner-item termination))
              (get *persons-resolved* outer-key))))
           endings-by-person-type)))

(def *rules-present* `(
   ;; Rule Simple Concatenation, spelling changes (-er) *****************
   ;; ...
   
   ;; Rule Simple Concatenation, special cases (-re)
   (~(fn[steam termination verb tense persons] 
       (and (some (fn[x] (= x verb)) '("rompre" "interrompre"))))
     ;; GRAMMAR: Ending in -re, in the 3rd person singular
     ;; GRAMMAR: these verbs have the particularity of just 
     ;; GRAMMAR: adding a 't' to the steam
    ~(fn[steam termination verb tense persons] 
      (persons-conjugator 
        steam 
        (endings-resolved (get *endings-present* termination)) 
        persons 
        {:il (.concat steam "t") 
         :elle (.concat steam "t") 
         :on (.concat steam "t")})))

   ;; Rule Simple Concatenation *****************************************
   ;; * Just adds terminations based on person / termination of infinitive
   (~(fn[steam termination verb tense persons]
       (= tense :present-tense))
   ~(fn[steam termination verb tense persons] 
      (persons-conjugator 
        steam 
        (endings-resolved (get *endings-present* termination)) 
        persons 
        {})))
   ))


;; END PRESENT *****************************************************


;; Rules for conjugating automatically regular verbs.
;; Transforming functions should parse a (steam termination verb person type) 
;; and return a conjugation  

(def *tense-to-rules* {
   :present-tense *rules-present*})

(defn regular-rule [verb tense] 
  ;; Extract steam
  ;; Get persons for tense
  ;; Evaluate rules for tense
  (let [y (verb-parts-extract verb)]	  
    (evaluate-rules-scalar
	    (concat y (list tense) (list (tense-to-persons tense)))
	    (get *tense-to-rules* tense))
    ))

;; ---------------