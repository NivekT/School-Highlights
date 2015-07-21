;; An exercise using Racket's built-in function to build
;; a poker card based on the input of number and suites.

#lang typed/racket
(require typed/test-engine/racket-tests)

(require/typed 2htdp/image
   [#:opaque Image image?]
   [rectangle (-> Number Number String String Image)]
   [image-width (-> Image Number)]
   [image-height (-> Image Number)]
   [text (-> String Number String Image)] 
   [beside (-> Image * Image)]
   [beside/align (-> String Image * Image)]
   [above (-> Image * Image)]
   [above/align (-> String Image * Image)]
   [overlay (-> Image * Image)]
   [crop (-> Number Number Number Number Image Image)]
   [flip-vertical (-> Image Image)]
   [flip-horizontal (-> Image Image)]
   [freeze (-> Image Image)]
   [rotate (-> Number Image Image)])

;; medium-sized icons for each suit

(: hearts-med Image)
(define hearts-med 
  (crop -2 10 32 36 (text "♥" 40 "red")))

(: spades-med Image)
(define spades-med 
  (crop -2 10 32 36 (text "♠" 40 "black")))

(: clubs-med Image)
(define clubs-med 
  (crop 0 10 32 36 (text "♣" 40 "black")))

(: diamonds-med Image)
(define diamonds-med 
  (crop -2 10 32 36 (text "♦" 40 "red")))

;; small icons for each suit

(: hearts-sm Image)
(define hearts-sm 
  (crop -2 4 18 20 (text "♥" 20 "red")))

(: spades-sm Image)
(define spades-sm 
  (crop -2 4 18 20 (text "♠" 20 "black")))

(: clubs-sm Image)
(define clubs-sm 
  (crop -1 4 18 20 (text "♣" 20 "black")))

(: diamonds-sm Image)
(define diamonds-sm 
  (crop -2 4 18 20 (text "♦" 20 "red")))

;; suit structures

(define-struct Suit
  ([name : String]
   [color : String]
   [small-icon : Image]
   [medium-icon : Image])
  #:transparent)

(define hearts   (make-Suit "hearts" "red" hearts-sm hearts-med))
(define diamonds (make-Suit "diamonds" "red" diamonds-sm diamonds-med))
(define clubs    (make-Suit "clubs" "black" clubs-sm clubs-med))
(define spades   (make-Suit "spades" "black" spades-sm spades-med))

;; some useful operations

(: frame (-> Integer Image Image))
;; given padding in pixels and an image, draw a thin
;; black rectangle around the image
(define (frame padding i)
  (overlay i (rectangle (+ padding (image-width i)) 
                        (+ padding (image-height i)) 
                        "outline" 
                        "black")))

(: spacer-v (-> Number Image))
;; construct a tall, thin, white rectangle for vertical space
(define (spacer-v n)
  (rectangle 1 n "solid" "white"))

(: flip-v (-> Image Image))
;; flip an image vertically, even if image includes text
;; (this is why "freeze" is called)
(define (flip-v i)
  (flip-vertical (freeze i)))

(: mirror-v (-> Image Image))
;; "vertical mirroring" -- show image above its own reflection
(define (mirror-v i)
  (above i (flip-v i)))

;; === student's code below ===
(: flip-h (-> Image Image))
;; flip an image horizontally, even if image includes text
;; This function is a modified version of given flip-v, not completely
;; my original work
(define (flip-h i)
  (flip-horizontal (freeze i)))

(: mirror-h (-> Image Image))
;; "horizontall mirroring" -- show image above its own reflection
;; This function is a modified version of given mirror-v, not completely
;; my original work
(define (mirror-h i)
  (beside i (flip-h i)))

(: my-spacer (-> Number Number Image))
;; construct a tall, thin, white rectangle for vertical space
;; given width and height
;; This function is a modified version of given spacer-v, not completely
;; my original work
;; The reason that I made my own is that the given does not allow me to control the width 
(define (my-spacer w h)
  (rectangle w h "solid" "white"))


(: nine-of (-> Suit Image))
;; Produces a poker card with the number 9 with a given suit
(define (nine-of suit)
  (merge-card 9 suit))

(: ten-of (-> Suit Image))
;; Produces a poker card with the number 10 with a given suit
(define (ten-of suit)
  (merge-card 10 suit))

(: merge-card (-> Integer Suit Image))
;; Merge the center and sides together
(define (merge-card n suit)
  (local
    {(define i (center-card n suit))
     ;; i is the center of the card with 9 or 10 suits
     (define space (my-spacer (image-width (num-si n suit)) 
                              (image-height i)))}
    ;; space is the vertical space below/above the number
    ;; and the small icon
    (match suit
      [(Suit name color s-i m-i)
       (frame 10 (beside (beside
                          (above (num-si n suit) 
                                 space)
                          i)
                         (above space
                                (flip-v (flip-h (num-si n suit))) 
                                )))])))

(: num-si (-> Integer Suit Image))
;; Generates a given number over a given small icon suit
(define (num-si n suit)
  (match suit
    [(Suit name color s-i m-i)
     (above (text (number->string n) 20 color)
            s-i)]))

(: center-card (-> Integer Suit Image))
;; Creates the center of the card with medium icons
(define (center-card n suit)
  (match suit
    [(Suit name color s-i m-i)
     (overlay-center n suit  
                     (mirror-h (beside (gen-card n suit) 
                                       (my-spacer 14 
                                                  (image-height (gen-card 
                                                                 n 
                                                                 suit))))))]))

(: overlay-center (-> Integer Suit Image Image))
;; Add medium icon(s) into the center
(define (overlay-center n suit i)
  (match suit
    [(Suit name color s-i m-i)
     (cond
       [(= n 9) (overlay (above m-i (rectangle 14 3 "solid" "white")) i)]
       [(= n 10) (overlay (mirror-v (above m-i (my-spacer (image-width m-i) 16))) i)]
       [else (error "Incorrect Input")])]))

(: gen-card (-> Integer Suit Image))
;; Generates an image with 4 medium icons
;; 2 of which mirror each other
(define (gen-card n suit)
  (match suit
    [(Suit name color s-i m-i)
     (mirror-v (above m-i (crop-medium-icon m-i)))]))

(: crop-medium-icon (-> Image Image))
;; Crop out 2 pixels of space from medium icons from suits
(define (crop-medium-icon mi)
  (crop 0 0 (image-width mi) (- (image-height mi) 7) mi))

;(test)