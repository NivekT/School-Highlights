;; Exercise with polymorphic binary search tree

#lang typed/racket
(require typed/test-engine/racket-tests)

;; === polymorphic data structures (broadly useful)

(define-struct (a b) Pair
  ([first  : a]
   [second : b])
  #:transparent)

(define-type (Maybe a) (U 'Nothing (Just a)))

(define-struct (a) Just
  ([x : a])
  #:transparent)

(define-type Order (U 'Less 'Equal 'Greater))

(define-type (Cmp a) (a a -> Order))

;; === BST map data structures

(define-struct (key val) BSTMap
  ([compare : (Cmp key)]
   [data    : (BST key val)])
  #:transparent)

(define-type (BST key val) (U 'E (Node key val)))
;; note the symbol 'E is used for the empty tree

(define-struct (key val) Node
  ([root : key]
   [v    : val]
   [lsub : (BST key val)]
   [rsub : (BST key val)])
  #:transparent)

;; Provided codes

(: cmp-int (Cmp Integer))
(define (cmp-int m n)
  (cond
    [(< m n) 'Less]
    [(= m n) 'Equal]
    [else 'Greater]))


;; Sample trees for testing
(define sample (make-BSTMap cmp-int (make-Node 2 "Two" 
                                               (make-Node 1 "One" 'E 'E)
                                               (make-Node 3 "Three" 'E 'E))))

(: emptytree (BSTMap Integer String))
(define emptytree (BSTMap cmp-int 'E))

(: fig1-tree (BST Integer String))
(define fig1-tree
  (Node 5 "Regenstein"
        (Node 1 "Ryerson"
              (Node 0 "Harper" 'E 'E)
              (Node 3 "Crerar" 'E 'E))
        (Node 8 "Pick"
              (Node 6 "Kent" 'E 'E)
              (Node 9 "Ratner" 'E 'E))))

(: fig1-map (BSTMap Integer String))
(define fig1-map
  (BSTMap cmp-int fig1-tree))

(: map-apply : (All (key val) (BSTMap key val) key -> (Maybe val)))
;; if the key is present in the tree, return the associated value in a Just;
;; otherwise, return 'Nothing
(define (map-apply m k)
  (match m
    [(BSTMap cmp t)
     (local
       {(: lp : (BST key val) -> (Maybe val))
        ;; search for k in t
        (define (lp t)
          (match t
            ['E 'Nothing]
            [(Node r v tl tr)
             (match (cmp k r)
               ['Less (lp tl)]
               ['Equal (Just v)]
               ['Greater (lp tr)])]))}
       (lp t))]))

(check-expect (map-apply sample 1) (Just "One"))
(check-expect (map-apply emptytree 1) 'Nothing)
(check-expect (map-apply fig1-map 0) (Just "Harper"))

(: insert : (All (key val) (BSTMap key val) key val -> (BSTMap key val)))
;; Insert the given key val into the correct place in a BSTMap
;; If the key already exists, replace the values with the new input
(define (insert m k v1)
  (match m
    [(BSTMap cmp t)
     (local
       {(: nd-creator : (BST key val) -> (BST key val))
        ;; Goes into (BST key val), determines if it is 'E or Node
        ;; if the former, creates a Node in that location
        ;; if the latter, makes a comparison, then recursively go down the tree
        ;; until it finds a proper location for input pairs of key and value
        (define (nd-creator t)
          (match t
            ['E (make-Node k v1 'E 'E)]
            [(Node r v2 tl tr)
             (match (cmp k r)
               ['Less (make-Node r v2 (nd-creator tl) tr)]
               ['Equal (make-Node k v1 tl tr)]
               ['Greater (make-Node r v2 tl (nd-creator tr))])]))}
       (make-BSTMap cmp (nd-creator t)))]))


(define sample-1 (Node 1 "One" 'E 'E))
(check-expect (BSTMap-data (insert emptytree 1 "One")) sample-1)

(define sample-2 (Node 2 "I just got replaced" 
                        (Node 1 "One" 'E 'E)
                        (Node 3 "Three" 'E 'E)))

(check-expect (BSTMap-data (insert sample 2 "I just got replaced"))
              sample-2)


(: remove-max : (All (key val) (BST key val) -> (Pair (Pair key val) (BST key val))))
;; Removes the pair (of key and value) with the maximum key
;; Returns the pair and the remaining tree without the pair
(define (remove-max t)
  (match t 
    ['E (error "The tree is empty. There is nothing to be removed")]
    [else 
    (local
      {(: remove : (BST key val) -> (BST key val))
       ;; Remove the branch with the greatest key from the tree
       (define (remove t)
         (match t
           ['E 'E]
           [(Node r v tl tr)
            (match tr
              ['E tl]
              [else (make-Node r v tl (remove tr))])]))
       (: extract : (BST key val) -> (Pair key val))
       ;; Extract the branch with the greatest key into a pair of key value
       (define (extract t)
         (match t
           [(Node r v tl tr)
            (match tr
              ['E (make-Pair r v)]
              [else (extract tr)])]))}
      (make-Pair (extract t) (remove t)))]))

(check-expect (make-Pair (make-Pair 3 "Three")
                         (Node 2 "Two" 
                               (Node 1 "One" 'E 'E)
                               'E))
              (remove-max (BSTMap-data sample)))

(check-expect (remove-max fig1-tree)
              (make-Pair (make-Pair 9 "Ratner")
                         (Node 5 "Regenstein"
                               (Node 1 "Ryerson"
                                     (Node 0 "Harper" 'E 'E)
                                     (Node 3 "Crerar" 'E 'E))
                               (Node 8 "Pick"
                                     (Node 6 "Kent" 'E 'E)
                                     'E))))
                         

(: remove-root : (All (key val) (BST key val) -> (BST key val)))
;; Returns empty if given an empty tree
;; Returns left subtree from right subtree is empty, and vice versa
;; Removes the root from the tree and returns the max key from the left subtree
(define (remove-root t)
  (match t
    ['E 'E]
    [(Node _ _ 'E tr) tr]
    [(Node _ _ tl 'E) tl]
    [(Node k v tl tr)
     (local 
       {(define x (remove-max tl))}
        ;; x is the output of remove-max of the left tree
        ;; consisting of the pair removed, and the remaining tree
        ;; we use x to rebuild a new tree with root replaced
     (make-Node (Pair-first (Pair-first x)) 
                (Pair-second (Pair-first x))
                (Pair-second x)
                tr))]))
(check-expect (remove-root (BSTMap-data emptytree))
              'E)

(check-expect (remove-root (BSTMap-data sample))
              (Node 1 "One"
                    'E
                    (Node 3 "Three" 'E 'E)))

(check-expect (remove-root fig1-tree)
              (Node 3 "Crerar"
                    (Node 1 "Ryerson"
                          (Node 0 "Harper" 'E 'E)
                          'E)
                    (Node 8 "Pick"
                          (Node 6 "Kent" 'E 'E)
                          (Node 9 "Ratner" 'E 'E))))


     
(: remove : (All (key val) (BSTMap key val) key -> (BSTMap key val)))
;; Remove the given key and its corresponding value from the tree
(define (remove m k)
  (if (eqv? (map-apply m k) 'Nothing)
      m
      (match m
        [(BSTMap cmp t)
         (local
           {(: locator : (BST key val) -> (BST key val))
            ;; finds the location of the key that matches the key from the
            ;; initial input and applies remove-root to that location
            (define (locator t)
              (match t
                ['E 'E]
                [(Node r v tl tr) 
                 (match (cmp k r)
                   ['Less (make-Node r v (locator tl) tr)]
                   ['Equal (remove-root t)]
                   ['Greater (make-Node r v tl (locator tr))])]))}
           (make-BSTMap cmp (locator t)))])))

(check-expect (BSTMap-data (remove sample 3))
              (Node 2 "Two"
                    (Node 1 "One" 'E 'E)
                    'E))

(check-expect (BSTMap-data (remove fig1-map 9))
              (Pair-second (remove-max fig1-tree)))

(check-expect (BSTMap-data (remove emptytree 1))
              'E)
  

(test)