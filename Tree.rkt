;; The rose-tree struct is a node with zero or more child.
;; This contains some exercises working with this type of tree.

#lang typed/racket
(require typed/test-engine/racket-tests)

(define-struct (T) Rose-Tree
    ([value : T]
     [kids  : (Listof (Rose-Tree T))])
    #:transparent)


(: int-tree : (Rose-Tree Integer))
;; Example for testing
(define int-tree
  (make-Rose-Tree
   4
   (list (make-Rose-Tree -1 empty)
         (make-Rose-Tree 12 empty)
         (make-Rose-Tree 42 empty))))

(: string-tree : (Rose-Tree String))
;; Example for testing
(define string-tree
  (make-Rose-Tree
   "AAA"
   (list (make-Rose-Tree "B" 
                         (list (make-Rose-Tree "CC" empty)
                               (make-Rose-Tree "D" empty)))
         (make-Rose-Tree "E"
                         (list (make-Rose-Tree "FFF" empty)))
         (make-Rose-Tree "G" empty))))

(: rose-size : (All (T) (Rose-Tree T) -> Natural))
;; Counts the number of nodes in a given Rose-Tree
(define (rose-size rt)
  (match rt
    [(Rose-Tree _ list) (+ 1 (sum2 (my-map rose-size list)))]
    [(Rose-Tree _ empty) 1]))

(: my-map (All (X) (-> ((Rose-Tree X) -> Natural) 
                       (Listof (Rose-Tree X)) 
                       (Listof Natural))))
;; Converts each node in the Rose-Tree into 1
(define (my-map f list)
  (match list
    ['() '()]
    [(cons hd tl) (cons (f hd) (my-map f tl))]))

(: sum2 (-> (Listof Natural) Natural))
;; Sum all the natural numbers in the list
(define (sum2 l)
  (match l
    ['() 0]
    [(cons hd tl) (+ hd (sum2 tl))]))

(check-expect (rose-size int-tree) 4)
(check-expect (rose-size string-tree) 7)
    
(: rose-height : (All (T) (Rose-Tree T) -> Natural))
;; Determines the height of a given Rose-Tree by sending the list of Rose-Tree k
;; to height-counter
(define (rose-height rt)
  (match rt
    [(Rose-Tree _ '()) 1]
    [(Rose-Tree _ ls) (+ 1 (height-count ls))]))

(: height-count (All (T) (Listof (Rose-Tree T)) -> Natural))
;; Counts height with rose-height recursively
(define (height-count lt)
  (match lt
    ['() 0]
    [(cons hd tl) (max (rose-height hd) (height-count tl))]))
;; I asked for advice from Yessi Somoza and she suggested the usage of max

(check-expect (rose-height int-tree) 2)
(check-expect (rose-height string-tree) 3)
  

(: rose-map : (All (S T) (S -> T) (Rose-Tree S) -> (Rose-Tree T)))
;; Maps a function over the values of a Rose-Tree and returns a new tree
(define (rose-map f rt)
  (match rt
    [(Rose-Tree v list) (make-Rose-Tree (f v) (my-map2 rose-map f list))]
    [(Rose-Tree v empty) (make-Rose-Tree (f v) empty)]))

(: my-map2 (All (S T) (-> ((S -> T) (Rose-Tree S) -> (Rose-Tree T))
                          (S -> T)
                          (Listof (Rose-Tree S)) 
                          (Listof (Rose-Tree T)))))
;; A map function that allows the function "f1" to be a higher-order function
;; f2 is merely a placeholder in this function, such that it can be recursively called
;; by rose-map
(define (my-map2 f1 f2 rt)
  (match rt
    ['() '()]
    [(cons hd tl) (cons (f1 f2 hd) (my-map2 f1 f2 tl))]))

(check-expect (rose-map add1 int-tree) 
              (make-Rose-Tree 5
                              (list (make-Rose-Tree 0 empty)
                                    (make-Rose-Tree 13 empty)
                                    (make-Rose-Tree 43 empty))))
              

(test)