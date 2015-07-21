;; Using the Monte Carlo approximation method to estimate the area of 
;; intersection of multiple circles


#lang typed/racket
(require typed/test-engine/racket-tests)

(define-struct Point
  ([x : Real]
   [y : Real])
  #:transparent)

(define A (make-Point 0 0))
(define B (make-Point 0.5 0))
(define C (make-Point 2.5 0))
(define D (make-Point 0.25 0.4330127018922193))
(define E (make-Point 0.25 -0.4330127018922193))
(define F (make-Point 0.5 0))
(define G (make-Point 0.25 0))

(define-struct Rectangle
  ([w : Real]
   [l : Real]
   [center : Point])
  #:transparent)

(define-struct Circle
  ([r : Real]
   [center : Point])
  #:transparent)

(define Circle-A (make-Circle 1 A))
(define Circle-B (make-Circle 1 B))
(define Circle-C (make-Circle 1 C))
(define Circle-D (make-Circle 1 D))
(define Circle-E (make-Circle 1 E))
(define Circle-F (make-Circle .75 F))
(define Circle-G (make-Circle 1.25 G))

(define no-circles '())
(define one-circle (list Circle-A))
(define two-circles (list Circle-A Circle-B))
(define two-plus-no-overlap (list Circle-A Circle-B Circle-C))
(define three-circles (list Circle-A Circle-B Circle-D))
(define four-circles (list Circle-A Circle-B Circle-D Circle-E))
(define two-diff-radii (list Circle-A Circle-F))
(define two-plus-total-overlap (list Circle-A Circle-B Circle-G))

(: throw-dart (-> Rectangle Point))
;; Consumes a Rectangle and produces a Point chosen uniformly 
;; at random within the Rectangle
(define (throw-dart r)
  (match r
    [(Rectangle w l p) (make-Point (+ (* (random) w) (/ (- w) 2))
                                   (+ (* (random) l) (/ (- l) 2)))]))

(define sample-throw (throw-dart (make-Rectangle 2 2 (make-Point 0 0))))
(check-range (Point-x sample-throw) -1 1)
(check-range (Point-y sample-throw) -1 1)

(: within-circle? (-> Circle Point Boolean))
;; Consumes a Circle, an a Point, and returns #t if and only if 
;; the specified Point is contained within the Circle
(define (within-circle? c p)
  (match c
    ([Circle r center] 
     (match center
       ([Point h k] 
        (>= (* r r) (+ (expt (- (Point-x p) h) 2) (expt (- (Point-y p) k) 2))))))))

(check-expect (within-circle? Circle-A A) #t)

(: bounding-box (-> (Listof Circle) Rectangle))
;; Consumes a list of Circles and produces the smallest Rectangle that 
;; encloses the Circles
(define (bounding-box lc)
  (local 
    {(define t (argmax max (map (lambda ([y : Real] [r : Real]) (+ y r)) 
                      (map Point-y (map Circle-center lc))
                      (map Circle-r lc))))
     (define b (argmin min (map (lambda ([y : Real] [r : Real]) (- y r)) 
                      (map Point-y (map Circle-center lc))
                      (map Circle-r lc))))
     (define r (argmax max (map (lambda ([x : Real] [r : Real]) (+ x r)) 
                      (map Point-x (map Circle-center lc))
                      (map Circle-r lc))))
     (define l (argmin min (map (lambda ([x : Real] [r : Real]) (- x r)) 
                      (map Point-x (map Circle-center lc))
                      (map Circle-r lc))))}
    (make-Rectangle (- r l)
                    (- t b) 
                    (make-Point (/ (+ r l) 2) (/ (+ t b) 2)))))

(check-expect (bounding-box one-circle) (make-Rectangle 2 2 A))

(: box-area (-> Rectangle Real))
;; Find the area of the bounding box
(define (box-area r)
  (match r
    [(Rectangle w l _) (* w l)]))

(check-expect (box-area (make-Rectangle 2 2 A)) 4)

(: area-of-intersection-monte-carlo (-> (Listof Circle) Integer Real))
;; Consumes two Circles, and a number of trials, and returns an 
;; approximation of the area of the intersection of the Circles
(define (area-of-intersection-monte-carlo lc t)
  (cond 
    [(empty? lc) 0]
    [else (* (/ (counter lc t 0) t) (box-area (bounding-box lc)))]))

(check-expect (area-of-intersection-monte-carlo no-circles 1000) 0)
(check-within (area-of-intersection-monte-carlo one-circle 200000) 
              3.141996 0.05)
(check-within (area-of-intersection-monte-carlo two-circles 200000) 
              2.152585 0.05)
(check-expect (area-of-intersection-monte-carlo two-plus-no-overlap 1000) 0)
(check-within (area-of-intersection-monte-carlo three-circles 200000) 
              1.763094819489706 0.05)
(check-within (area-of-intersection-monte-carlo four-circles 200000) 
              1.3856659622217002 0.05)
(check-within (area-of-intersection-monte-carlo two-diff-radii 200000) 
              1.4765130 0.05)
(check-within (area-of-intersection-monte-carlo two-plus-total-overlap 200000) 
              2.1557875 0.05)

(: counter (-> (Listof Circle) Integer Integer Integer))
;; A recursive function that simulates the dart throwing trials for t times,
;; counts number of success in the process, and returns an estimation of the
;; overlapping area of circles
(define (counter lc t count)
  (local
    {(define p (throw-dart (bounding-box lc)))} 
    (cond
      [(= t 0) count]
      [(andmap (lambda ([c : Circle]) (within-circle? c p)) lc)
       (counter lc (- t 1) (+ 1 count))]
      [else (counter lc (- t 1) count)])))

(check-within (counter one-circle 100000 0) 78539 500)

(test)