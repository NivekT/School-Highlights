;; Using the provided data, create a stimulation of the Presidential
;; election. Then, use Google Image Charts API to show the result as 
;; an image.


#lang typed/racket

(require typed/test-engine/racket-tests)


(require/typed 2htdp/image
   [#:opaque Image image?]
   [text (-> String Number String Image)]
   [beside (-> Image * Image)]
   [above (-> Image * Image)]
   [bitmap/url (-> String Image)])

;; === data definitions ===

(define-type State 
  (U 'AL 'AK 'AZ 'AR 'CA 'CO 'CT 'DE 'DC 'FL 'GA 'HI 'ID 'IL 'IN 'IA 'KS
     'KY 'LA 'ME 'MD 'MA 'MI 'MN 'MS 'MO 'MT 'NE 'NV 'NH 'NJ 'NM 'NY 'NC
     'ND 'OH 'OK 'OR 'PA 'RI 'SC 'SD 'TN 'TX 'UT 'VT 'VA 'WA 'WV 'WI 'WY))
;; 51 state-like entities -- includes Washington, DC

(define-type Party
  (U 'D 'R)) ;; Democrats, Republicans
             ;; apologies to third parties! they're not represented.

(define-struct EV
  ([s  : State]    ;; a state symbol
   [ev : Integer]) ;; electoral votes per this state
  #:transparent)

(define-struct DemProb
  ([s    : State]
   [demp : Real])   ;; probability of Democratic victory on [0.0,1.0] 
  #:transparent)

(define-struct StateResult
  ([s  : State]
   [p  : Party]     ;; winning party
   [ev : Integer])  ;; number of electoral votes for victor
  #:transparent)

(define-struct USAResult
  ([dems : (Listof StateResult)]  ;; states won by Democrats
   [reps : (Listof StateResult)]) ;; states won by Republicans
  #:transparent)

(define-struct Tally
  ([demv : Integer]  ;; simulations where D candidate wins
   [repv : Integer]  ;; simulations where R candidate wins
   [ties : Integer]) ;; simulations where candidates tie
  #:transparent)

(define-type Outcome (U Party 'tie))

;;; === data === 

(: ev-map (Listof EV))
(define ev-map 
  (list
   (EV 'AL 9)
   (EV 'AK 3)
   (EV 'AZ 11)
   (EV 'AR 6)
   (EV 'CA 55)
   (EV 'CO 9)
   (EV 'CT 7)
   (EV 'DE 3)
   (EV 'DC 3)
   (EV 'FL 29)
   (EV 'GA 16)
   (EV 'HI 4)
   (EV 'ID 4)
   (EV 'IL 20)
   (EV 'IN 11)
   (EV 'IA 6)
   (EV 'KS 6)
   (EV 'KY 8)
   (EV 'LA 8)
   (EV 'ME 4)
   (EV 'MD 10)
   (EV 'MA 11)
   (EV 'MI 16)
   (EV 'MN 10)
   (EV 'MS 6)
   (EV 'MO 10)
   (EV 'MT 3)
   (EV 'NE 5)
   (EV 'NV 6)
   (EV 'NH 4)
   (EV 'NJ 14)
   (EV 'NM 5)
   (EV 'NY 29)
   (EV 'NC 15)
   (EV 'ND 3)
   (EV 'OH 18)
   (EV 'OK 7)
   (EV 'OR 7)
   (EV 'PA 20)
   (EV 'RI 4)
   (EV 'SC 9)
   (EV 'SD 3)
   (EV 'TN 11)
   (EV 'TX 38)
   (EV 'UT 6)
   (EV 'VT 3)
   (EV 'VA 13)
   (EV 'WA 12)
   (EV 'WV 5)
   (EV 'WI 10)
   (EV 'WY 3)))

(: prob-map (Listof DemProb))
;; These probabilities are fabricated. They are loosely modeled on the 
;; Obama/Romney predictions prior to 2012 elections.
(define prob-map
  (list
   (DemProb 'AL 0)
   (DemProb 'AK 0)
   (DemProb 'AZ 0.02)
   (DemProb 'AR 0)
   (DemProb 'CA 1)
   (DemProb 'CO 0.50)
   (DemProb 'CT 1)
   (DemProb 'DE 1)
   (DemProb 'DC 1)
   (DemProb 'FL 0.30)
   (DemProb 'GA 0)
   (DemProb 'HI 1)
   (DemProb 'ID 0)
   (DemProb 'IL 1)
   (DemProb 'IN 0)
   (DemProb 'IA 0.73)
   (DemProb 'KS 0)
   (DemProb 'KY 0)
   (DemProb 'LA 0)
   (DemProb 'ME 0.89)
   (DemProb 'MD 1)
   (DemProb 'MA 1)
   (DemProb 'MI 0.80)
   (DemProb 'MN 0.94)
   (DemProb 'MS 0)
   (DemProb 'MO 0.23)
   (DemProb 'MT 0)
   (DemProb 'NE 0)
   (DemProb 'NV 0.65)
   (DemProb 'NH 0.70)
   (DemProb 'NJ 1)
   (DemProb 'NM 0.87)
   (DemProb 'NY 1)
   (DemProb 'NC 0.20)
   (DemProb 'ND 0)
   (DemProb 'OH 0.50)
   (DemProb 'OK 0)
   (DemProb 'OR 0.90)
   (DemProb 'PA 0.72)
   (DemProb 'RI 1)
   (DemProb 'SC 0)
   (DemProb 'SD 0)
   (DemProb 'TN 0)
   (DemProb 'TX 0.01)
   (DemProb 'UT 0)
   (DemProb 'VT 1)
   (DemProb 'VA 0.50)
   (DemProb 'WA 1)
   (DemProb 'WV 0)
   (DemProb 'WI 0.68)
   (DemProb 'WY 0.02)))
  
(: all-states (Listof State))
(define all-states
  (list 'AL 'AK 'AZ 'AR 'CA 'CO 'CT 'DE 'DC 'FL 'GA 'HI 'ID 'IL 'IN 'IA 'KS
        'KY 'LA 'ME 'MD 'MA 'MI 'MN 'MS 'MO 'MT 'NE 'NV 'NH 'NJ 'NM 'NY 'NC
        'ND 'OH 'OK 'OR 'PA 'RI 'SC 'SD 'TN 'TX 'UT 'VT 'VA 'WA 'WV 'WI 'WY))

;;; === simulation code === from lab5

(: sim-state : State -> StateResult)
;; given a state, choose a random number on [0,1] and consult 
;; the probability in prob-map above to determine victorious party
;; and look up the number of electoral votes in ev-map
(define (sim-state s)
  (local
    {(define demp-l (DemProb-demp (first (filter 
                            (lambda 
                                ([d : DemProb]) 
                              (eqv? s (DemProb-s d))) 
                            prob-map))))
     ;; demp-l is the probability that the Democrats wil win in the given state
     ;; which is extracted from prob-map
     (define ev-l (EV-ev (first (filter (lambda ([v : EV])
                                  (eqv? s (EV-s v)))
                                ev-map))))}
    ;; ev-l is the electoral votes in the given state
    ;; which is extracted from ev-map
    (if (< (random) demp-l) 
        (make-StateResult s 'D ev-l) 
        (make-StateResult s 'R ev-l))))

;(check-expect (sim-state 'CA) (make-StateResult 'CA 'D 55)) 
;; Democrats always win in California


(: sim-USA : -> USAResult)
;; run simulation on all states (plus Washington, DC)
(define (sim-USA)
  (local
    {(define ls-sr (map sim-state all-states))
     ;; List of all StateResult
     (define d (filter-map (lambda ([sr : StateResult]) 
                             (and (eqv? (StateResult-p sr) 'D) sr)) 
                           ls-sr))
     ;; List of StateResult where the Democrats won by reading the structure
     ;; and filtering out the ones where (StateResult-p sr) =/= 'D
     (define r (remv* d ls-sr))}
     ;; List of StateResult where the Republicans won by removing items
     ;; in "d" from "ls-sr"
    (make-USAResult d r)))

;(sim-USA) ;; Shows that it works
  
(: outcome : USAResult -> Outcome)
;; Add the electoral votes of each candidate to determine outcome.
;; Assume no state splits its electoral votes (in actuality, some do).
(define (outcome r)
  (local
    {(define d-list (USAResult-dems r))
     ;; The list of StateResult where the Democrats won
     (define d-votes (foldr + 0 (map StateResult-ev d-list)))}
    ;; Calculates the number of electoral votes that the Democrats have
    (cond
      [(> d-votes 269) 'D]
      [(= d-votes 269) 'tie]
      [else 'R])))

;(outcome (sim-USA)) ;; Shows that it works


;; Generate the image

(: invalid-state? : StateResult -> Boolean)
;; check if the StateResult belongs to DC, HI. or AK
(define (invalid-state? sr)
  (match sr
    [(StateResult s _ _)
     (or (eqv? (StateResult-s sr) 'DC) 
         (eqv? (StateResult-s sr) 'HI) 
         (eqv? (StateResult-s sr) 'AK))]))

(check-expect (invalid-state? (StateResult 'CA 'D 55)) #f)
(check-expect (invalid-state? (StateResult 'AK 'R 3)) #t)

(: party->URL : (Listof StateResult) -> String)
;; Given a list of StateResults that the party has won
;; return it in the form of string (in URL format)
(define (party->URL sr)
  (match sr
    ['() ""]
    [(cons h t) 
     (match h
       [(StateResult s _ _)
        (if (eqv? t '())
            (string-append "US-" (symbol->string s) (party->URL t))
            (string-append "US-" (symbol->string s) "|" (party->URL t)))])]))

(check-expect (party->URL (list (StateResult 'CA 'D 55))) "US-CA")
(check-expect (party->URL (list (StateResult 'CA 'D 55)
                                (StateResult 'CO 'D 9))) "US-CA|US-CO")




(: sim-image-URL : USAResult -> String)
;; Consumes a USAResult and returns a Google Image URL that displays
;; the results of 48 states on a map with blue representing the Democrats,
;; and red representing the Republicans
(define (sim-image-URL USA)
  (match USA
    [(USAResult dem rep)
     (local
       {(define d 
          (filter (lambda ([sr : StateResult]) (not (invalid-state? sr)))
                  dem))
        (define r 
          (filter (lambda ([sr : StateResult]) (not (invalid-state? sr)))
                  rep))
        (: state-colors : Integer Integer -> String)
        ;; Given the number of states that the Democrats have won (w/o DC, HI, or AK)
        ;; returns the string (in URL format) that will give the states the corresponding colors on the map
        (define (state-colors i count)
          (cond
            [(= count 48) ""]
            [(< 0 i) (string-append "|0000FF" (state-colors (- i 1) (+ 1 count)))]
            [else (string-append "|FF0000" (state-colors i (+ 1 count)))]))}
       (string-append "http://chart.googleapis.com/"
                      "chart?cht=map:auto=20,20,40,40&chs=240x240&chld="
                      (party->URL d) "|" (party->URL r)
                      "&chco=ABABAB"
                      (state-colors (length d) 0)
                      "&chtt=Simulated%202016%20Election"))]))

;; The string result is random, therefore, we will do an eyeball check
(sim-image-URL (sim-USA)) ;; Looks right!


(: sim-image : USAResult -> Image)
;; Produces an image that contains the map of the election results
;; and the outcomes for DC, HI and AK, the number of electoral votes won by each party
;; and the overall outcome
(define (sim-image USA)
  (match USA
    [(USAResult dem rep)
     (local
       {(define filtered-states
          (append (filter (lambda ([sr : StateResult]) (invalid-state? sr)) dem)
                  (filter (lambda ([sr : StateResult]) (invalid-state? sr)) rep)))
        
        (: state-result-text : (Listof StateResult) -> Image)
        ;; Create texts for the results of states not included on the map
        (define (state-result-text ls)
          (match ls
            ['() (error "not supposed to happen")] ;; I wish i could use empty-image here
            [(cons h t) (if (eqv? t '())
                            (match h
                              [(StateResult s p _)
                               (text (string-append (match p
                                                      ['D "Democrats"]
                                                      ['R "Republicans"])
                                                    " win in "
                                                    (match s
                                                      ['AK "Alaska"]
                                                      ['DC "Washington, DC"]
                                                      ['HI "Hawaii"]))
                                     10 "black")])
                            (above (match h
                                     [(StateResult s p _)
                                      (text (string-append (match p
                                                             ['D "Democrats"]
                                                             ['R "Republicans"])
                                                           " win in "
                                                           (match s
                                                             ['AK "Alaska"]
                                                             ['DC "Washington, DC"]
                                                             ['HI "Hawaii"]))
                                            10 "black")])
                                   (state-result-text t)))]))
        
        (define d-ev (foldr + 0 (map StateResult-ev dem)))
        
        (define r-ev (foldr + 0 (map StateResult-ev rep)))}
       (above (bitmap/url (sim-image-URL USA))
              (state-result-text filtered-states)
              (text (string-append "Democrats' Electoral Votes: " (number->string d-ev)) 13 "black")
              (text (string-append "Republicans' Electoral Votes: " (number->string r-ev)) 13 "black")
              (text (match (outcome USA)
                      ['tie "Tie!"]
                      ['D "Democrats Win!"]
                      ['R "Republicans Win!"])
                    18 "black")))]))

;; Eyeball Check
(sim-image (sim-USA)) ;; Eyeball Check

(: iterate-until-tie : -> Image)
;; run simulations repeatedly until the result is a tie
;; then displays the result's image
(define (iterate-until-tie)
  (local
    {(define result (sim-USA))
     (define oc (outcome result))}
    (match oc
        ['tie (sim-image result)]
        [else (iterate-until-tie)])))

;; Eyeball Check
(iterate-until-tie) ;; Eyeball Check










     
            
             
             
         
         
         
         
         
         
         
         
         
         
         
         
         


(test)