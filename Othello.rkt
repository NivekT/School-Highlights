#lang typed/racket
(require typed/test-engine/racket-tests)

;; The first part of the code consist of the infrastructure of a program to
;; play the board game Reversi (also known as Othello). It builds an image
;; of a 8 x 8 board with black and white pieces in the center.

;; The second part of the code allows the user to input board positions to
;; play the game. It also has an AI that evaluates the current board situation
;; and play according to heuristics/strategies coded by me.


;; Codes from ProjA Sample

(require/typed 2htdp/image
               [#:opaque Image image?]
               [overlay (Image Image -> Image)]
               [empty-image Image]
               [circle (Real String String -> Image)]
               [square (Real String String -> Image)]
               [rectangle (Real Real String String -> Image)]
               [text (String Integer String -> Image)] 
               [beside/align (String Image * -> Image)]
               [above/align (String Image * -> Image)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS

;; game players
(define-type Player (U 'black 'white))

;; board positions
(define-struct Pos
  ([row : Integer]  ;; an integer on the interval [0,7]
   [col : Integer]) ;; an integer on the interval [0,7]
  #:transparent)

;; possible contents of a square on the board
(define-type Cell (U Player 'none))

;; game board
(define-struct Board
  ([squares : (Listof Cell)]) ;; a list of length 64
  #:transparent)

;; game state
(define-struct Game
  ([board : Board]
   [next  : Player])
  #:transparent)

;; direction
(define-struct Dir
  ([dr : (U -1 0 1)]
   [dc : (U -1 0 1)])
  #:transparent)

;; outcome of a game
(define-type Outcome (U Player 'tie))

;; maybe type
(define-type (Maybe A) (U 'Nothing (Just A)))
(define-struct (A) Just ([value : A]) #:transparent)

;; pairs
(define-struct (A B) Pair ([fst : A] [snd : B]) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

;; index offsets for the 8-neighborhood of a square
;;
(define neighbor-offsets
  (list (Dir -1 -1) (Dir -1 0) (Dir -1 1)
        (Dir 0 -1) (Dir 0 1)
        (Dir 1 -1) (Dir 1 0) (Dir 1 1)))

;; width of the board in squares
(define board-wid 8)

;; number of squares on the board
(define num-cells (* board-wid board-wid))

;; lower-bound on display width of a single cell
(define minimum-cell-wid 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UTILITY FUNCTIONS

(: other-player : Player -> Player)
;; return the other player
(define (other-player player)
  (match player ['white 'black] ['black 'white]))

(: same-player : Player -> (Cell -> Boolean))
;; curried function for testing if a cell holds a player's piece
(define (same-player player)
  (λ ([p : Cell]) (symbol=? player p)))

(: player->string : Player -> String)
;; return the name of the player
(define (player->string player)
  (match player ['black "Black"] ['white "White"]))

(: is-empty? : Cell -> Boolean)
;; is a cell value 'none?
(define (is-empty? cell) (symbol=? cell 'none))

(: on-board? : Pos -> Boolean)
;; is a position on the board?
(define (on-board? p)
  (match p [(Pos r c) (and (<= 0 r 7) (<= 0 c 7))]))

(: pos->index : Pos -> Integer)
;; convert a position to an index (0..63)
(define (pos->index p)
  (match p [(Pos row col) (+ (* board-wid row) col)]))

(: index->pos : Integer -> Pos)
;; convert an index to a position
(define (index->pos idx)
  (Pos (quotient idx 8) (remainder idx 8)))

(: cells-update : (Listof Cell) Integer Cell -> (Listof Cell))
;; functional update an element of a list of cells
(define (cells-update cells idx v)
  (local
    {(: update : Integer (Listof Cell) -> (Listof Cell))
     (define (update i cells)
       (match* (i cells)
         [(0 (cons hd tl)) (cons v (rest cells))]
         [(i (cons hd tl)) (cons hd (update (- i 1) tl))]
         [(_ _) (error 'cells-update "invalid index")]))}
    (update idx cells)))

(: board-update : Pos Cell Board -> Board)
;; functional update of a board
(define (board-update p v brd)
  (match brd [(Board cells) (make-Board (cells-update cells (pos->index p) v))]))

(: cell-at : Board Integer Integer -> Cell)
;; return the cell value at the given row and column
(define (cell-at brd r c)
  (match brd [(Board cells) (list-ref cells (+ (* board-wid r) c))]))

(: board-ref : Board Pos -> Cell)
;; return the cell value at the given position
(define (board-ref brd p)
  (match brd [(Board cells) (list-ref cells (pos->index p))]))

(: pos+ : Pos Dir -> Pos)
;; add two positions
(define (pos+ p dir)
  (match* (p dir) [((Pos pr pc) (Dir dr dc)) (make-Pos (+ pr dr) (+ pc dc))]))

(: count-pieces : Board Player -> Integer)
;; count the pieces on the board belonging to the player
(define (count-pieces brd player)
  (foldl
   (λ ([cell : Cell] [sum : Integer])
     (if (symbol=? cell player) (+ sum 1) sum))
   0
   (Board-squares brd)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VISUALIZATION

(: build-row : (Integer -> Image) -> Image)
;; glue eight images left-to-right using the supplied function to create the
;; images
(define (build-row mkImg)
  (foldl
   (λ ([img : Image] [acc : Image]) (beside/align "middle" acc img))
   empty-image
   (build-list board-wid mkImg)))

(: build-col : (Integer -> Image) -> Image)
;; glue eight imagestop-to-bottom using the supplied function to create the
;; images
(define (build-col mkImg)
  (foldl
   (λ ([img : Image] [acc : Image]) (above/align "middle" acc img))
   empty-image
   (build-list board-wid mkImg)))

(: game-image : Game Integer -> Image)
;; render the game state as an image of the given width
(define (game-image game total-wid)
  (local
    {(define cell-wid (/ total-wid (+ board-wid 1)))
     (define font-sz (exact-floor cell-wid))
     (: text2 : String String -> Image)
     (define (text2 s1 s2) (text (string-append s1 " " s2) font-sz "Black"))}
    (if (< cell-wid minimum-cell-wid)
        (error 'game-image "total width too small")
        (match game
          [(Game brd player)
           (above/align
            "left"
            (draw-board cell-wid brd)
            (rectangle total-wid 5 "solid" "white")
            (text2 (player->string player) "is next")
            (text2 "Black's score:" (number->string (count-pieces brd 'black)))
            (text2 "White's score:" (number->string (count-pieces brd 'white))))]))))

(: board-image : Board Integer -> Image)
;; render the board state as an image of the given width
(define (board-image brd total-wid)
  (local
    {(define cell-wid (/ total-wid (+ board-wid 1)))}
    (if (< cell-wid minimum-cell-wid)
        (error 'game-image "total width too small")
        (draw-board cell-wid brd))))

(: draw-board : Real Board -> Image)
;; helper function that actually draws a board given the width of a square.
(define (draw-board cell-wid brd)
  (local
    {(define box (square cell-wid "outline" "black"))
     (define empty-sq (overlay box (square cell-wid "solid" "darkgreen")))
     (define black-sq (overlay (circle (/ (- cell-wid 6) 2) "solid" "black") empty-sq))
     (define white-sq (overlay (circle (/ (- cell-wid 6) 2) "solid" "white") empty-sq))
     (define font-sz (exact-floor (- cell-wid 4)))
     (: label-sq : Integer -> Image)
     (define (label-sq i) (overlay (text (number->string i) font-sz "black") box))
     (: cell->image : Cell -> Image)
     (define (cell->image cell)
       (match cell
         ['none empty-sq]
         ['black black-sq]
         ['white white-sq]))
     (: mk-row : Integer -> Image)
     (define (mk-row r)
       (build-row (λ [(c : Integer)] (cell->image (cell-at brd r c)))))}
    (beside/align
     "bottom"
     (build-col label-sq)
     (above/align "middle" (build-row label-sq) (build-col mk-row)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GAME LOGIC

;; initial state of the game board
(: new-board : Board)
(define new-board
  (board-update
   (make-Pos 3 4) 'black
   (board-update
    (make-Pos 4 3) 'black
    (board-update
     (make-Pos 3 3) 'white
     (board-update
      (make-Pos 4 4) 'white
      (make-Board (make-list num-cells 'none)))))))

;; initial state of the game
(: new-game : Game)
(define new-game (make-Game new-board 'black))

(: flips : Board Player Pos -> (Listof Pos))
;; return a list of the squares that are flipped if the player places a piece
;; at the given position.  The result does _not_ include the played piece.
;; The empty list is returned if the move is not legal.
(define (flips brd player p)
  (local
    {(: f : Dir (Listof Pos) -> (Listof Pos))
     (define (f dir cells) (append (try-flip-in-dir brd player p dir) cells))}
    (if (and (on-board? p) (is-empty? (board-ref brd p)))
        (foldl f '() neighbor-offsets)
        '())))

(: try-flip-in-dir : Board Player Pos Dir -> (Listof Pos))
;; given a board, player, starting position, and direction, try to flip pieces
;; We assume that the initial pos is empty and on the board
(define (try-flip-in-dir brd player start dir)
  (local
    {(define p1 (pos+ start dir))
     (define is-other? (same-player (other-player player)))
     (: try-flip : Pos (Listof Pos) -> (Listof Pos))
     ;; flip opponent's pieces in direction dir until we hit one of player's
     ;; pieces.  Return the empty list if we cannot flip legally
     (define (try-flip p ps)
       (cond
         [(not (on-board? p)) '()]
         [(is-other? (board-ref brd p)) (try-flip (pos+ p dir) (cons p ps))]
         [(is-empty? (board-ref brd p)) '()]
         [else ps]))}
    (try-flip p1 '())))

(: outflanks? : Board Player Pos -> Boolean)
;; return true if the player can leagally place a piece on the board at
;; the given location.
(define (outflanks? brd player p)
  (local
    {(: f : Dir -> Boolean)
     (define (f dir) (cons? (try-flip-in-dir brd player p dir)))}
    (and (on-board? p)
         (is-empty? (board-ref brd p))
         (ormap f neighbor-offsets))))

(: board-apply-move : Board Player Pos -> Board)
;; apply a move to board; signal an error if the move is not legal
(define (board-apply-move brd player p)
  (match (flips brd player p)
    ['() (error 'apply-move "illegal move")]
    [ps (local
          {(: apply-flip : Pos Board -> Board)
           (define (apply-flip q brd) (board-update q player brd))}
          (foldl apply-flip brd (cons p ps)))]))

(: apply-move : Game Player Pos -> Game)
;; apply a move to a game state; signal an error if the move is not legal
(define (apply-move g player pos)
  (match g
    [(Game brd next)
     (if (symbol=? next player)
         (Game (board-apply-move brd player pos) (other-player next))
         (error 'apply-move "not your turn to move"))]))

(: move-possible? : Board Player -> Boolean)
;; is it possible for the player to move on the given board?
(define (move-possible? brd player)
  (local
    {(define cells (Board-squares brd))
     (: search : Integer -> Boolean)
     ;; search the board squares looking for an empty square where the
     ;; player can make a legal move (i.e., outflank the opponent).
     (define (search idx)
       (if (< idx num-cells)
           (or (outflanks? brd player (index->pos idx)) (search (+ idx 1)))
           #f))}
    (search 0)))

(: game-over? : Game -> Boolean)
;; is the game over?
(define (game-over? g)
  (match g
    [(Game brd _) (not (or (move-possible? brd 'white)
                           (move-possible? brd 'black)))]))

(: outcome : Game -> Outcome)
;; determine the outcome of a game
(define (outcome g)
  (local
    {(define brd (Game-board g))
     (define white-score (count-pieces brd 'white))
     (define black-score (count-pieces brd 'black))}
    (cond
      [(< white-score black-score) 'black]
      [(> white-score black-score) 'white]
      [else 'tie])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ProjB


(define end-board 
  (board-update
   (make-Pos 7 7) 'white
   (board-update
    (make-Pos 0 7) 'none
    (board-update
     (make-Pos 7 4) 'none
     (make-Board (make-list num-cells 'black))))))

(: end-game : Game)
(define end-game (make-Game end-board 'white))


(define-type Strategy (Game -> Pos))


(: first-move Strategy)
;; Chooses the first legal move given a Game, starts from the top left croner
;; moves right til the end of the row, then moves down a row, and repeats
(define (first-move g)
  (match g
    [(Game b pl)
     (local
       {(: board-walker : Integer -> Pos)
        ;; Given an index position, check if the move is legal
        ;; if so, return the Pos, if not, adds 1 to index and checks again
        (define (board-walker idx)
          (local {(define po (index->pos idx))}
            (if (> idx 63)
                (error "No move is legal")
                (if (outflanks? b pl po)
                    po
                    (board-walker (+ 1 idx))))))}
       (board-walker 0))]))

(check-expect (first-move new-game) (Pos 2 3))
(check-expect (first-move (make-Game new-board 'white)) (Pos 2 4))

(: human Strategy)
;; Asks the user for an input of Pos until a valid one is received
;; if given EOF, returns an error
(define (human g)
  (local {(define input (read-line))}
    (cond 
      [(eof-object? input) (error "The input is EOF")]
      [else (local
              {(define result (parse-pos input))}
              (match result
                [(Pos x y) (Pos x y)]
                [string (human g)]))])))

(: parse-pos : String -> (U Pos String))
;; if the input is valid, returns creates a POs
;; if not, returns the invalid input as a string
(define (parse-pos s)
  (local {(define ls (string->list s))}
    (if (= (length ls) 2)
        (local {(define fir (first ls))
                (define sec (second ls))}
          (if (and (char-numeric? fir)
                   (char-numeric? sec))
              (Pos (- (char->integer fir) 48)
                   (- (char->integer sec) 48))
              s))
        s)))

;; manually tested (human new-game) here

(: play-loop : Game Strategy Strategy -> (Pair (Listof (Pair Player Pos)) Game))
;; given game, strategies of blk and wht
;; return a list of moves and the final state of game
(define (play-loop g blk-s wht-s)
  (local
    {(: lp : Game Strategy Strategy (Listof (Pair Player Pos)) Integer -> (Pair (Listof (Pair Player Pos)) Game))
     (define (lp g blk-s wht-s acc error-count)
       ;; a loop with two accumlators: the first contains past moves by the players
       ;; the latter counts the number of consecutive invalid moves, when the error = 3
       ;; terminates the game
       ;; otherwise, the loop asks a player for a move, applies it and display an image
       ;; then asks the otherplayer for a move recursively until the game is over
       (if (= error-count 3)
           (error "Game is terminated due to 3 consective errors")
           (begin (display (game-image g 200 ))
                  (if (game-over? g)
                      (Pair acc g)
                      (match g
                        [(Game b pl)
                         (local
                           {(define oppo (match pl ['black 'white] ['white 'black]))}
                           (cond
                             [(not (move-possible? b pl))
                              (lp (Game b oppo) blk-s wht-s acc 0)]
                             [else 
                              (local
                                {(define po (match pl ['black (blk-s g)] 
                                              ['white (wht-s g)]))}
                                ;; this match returns a Pos
                                (if (outflanks? b pl po) 
                                    (lp (apply-move g pl po) 
                                        blk-s 
                                        wht-s 
                                        (cons (Pair pl po) acc) 0)
                                    (begin (display "The move is illegal")
                                           (lp g blk-s wht-s 
                                               acc (+ 1 error-count)))))]))])))))}
    (lp g blk-s wht-s '() 0)))

(: play-loop-forgetful : Game Strategy Strategy -> (U Player 'tie))
;; "forgets" the history of the game; good for quick and easy console play
(define (play-loop-forgetful g b w)
  (match (play-loop g b w)
    [(Pair history game-at-end) (outcome game-at-end)]))

(: pass-and-play : -> (Pair (Listof (Pair Player Pos)) Game))
;; starts a new game
(define (pass-and-play)
  (play-loop new-game human human))

;; List of Pos by types
(define all-pos (map index->pos (build-list 64 (λ ([idx : Integer]) idx))))
(define edg-pos (filter (λ ([po : Pos]) ;; contains corners
                          (or (= (Pos-row po) 0) (= (Pos-col po) 0)
                              (= (Pos-row po) 7) (= (Pos-col po) 7))) all-pos))
(define cor-pos (list (Pos 0 0) (Pos 0 7) (Pos 7 0) (Pos 7 7)))
;; these lists are sorted in the sense of "first-move"

(: ls-of-pos-moves : (Listof Pos) Game -> (Listof Pos))
;; Given a list of Pos, returns a list of possible moves
(define (ls-of-pos-moves ls g)
  (match g
    [(Game b pl)
     (filter-map (λ ([po : Pos]) (and (outflanks? b pl po) po)) ls)]))

(check-expect (ls-of-pos-moves all-pos new-game) (list (Pos 2 3)(Pos 3 2)
                                                       (Pos 4 5)(Pos 5 4)))
(check-expect (ls-of-pos-moves all-pos end-game) (list (Pos 0 7) (Pos 7 4)))


(: immediate-tactics : Strategy)
;; Chooses corners over edges, edges over the rest
;; if there is more than one move of the same type available 
;; chooses the one that returns the most flips
(define (immediate-tactics g)
  (match g
    [(Game b pl)
     (local             
       {(: most-flips : (Listof Pos) -> Pos)
        ;; given a list of possible moves, returns the move that flips
        ;; the most opponent pieces
        (define (most-flips ls)
          (argmax (λ ([x : Pos]) (length (flips b pl x))) ls))
        
        (define corner-moves (ls-of-pos-moves cor-pos g))
        (define edge-moves (ls-of-pos-moves edg-pos g))
        (define all-moves (ls-of-pos-moves all-pos g))}
       (cond
         [(not (empty? corner-moves)) (most-flips corner-moves)]
         [(not (empty? edge-moves)) (most-flips corner-moves)]
         [else (most-flips all-moves)]))]))

(define-type Heuristic (Game -> Integer))

(: piece-counting : Heuristic)
;; number of black pieces minus number of white pieces
(define (piece-counting g)
  ((type-count 1 1) g))

(check-expect (piece-counting new-game) 0)

(: type-count : Integer Integer -> Heuristic)
;; Given two weight multiplers for edge and corner pieces and a Game
;; returns a Heuristic which counts the number of black pieces 
;; minus number of white pieces with weighting included
(define (type-count e c)
  (λ 
      ([g : Game]) 
    (match g
      [(Game b _)
       (local 
         {(define corners (map (λ ([po : Pos]) (board-ref b po)) cor-pos)) 
          (define edges (map (λ ([po : Pos]) (board-ref b po)) edg-pos))
          (define all (map (λ ([po : Pos]) (board-ref b po)) all-pos))
          (define blk-all (count (λ ([c : Cell]) (equal? 'black c)) all))
          (define wht-all (count (λ ([c : Cell]) (equal? 'white c)) all))
          (define blk-edg (count (λ ([c : Cell]) (equal? 'black c)) edges))
          (define wht-edg (count (λ ([c : Cell]) (equal? 'white c)) edges))
          (define blk-cor (count (λ ([c : Cell]) (equal? 'black c)) corners))
          (define wht-cor (count (λ ([c : Cell]) (equal? 'white c)) corners))}
         (match* (e c)
           [(1 1) (- blk-all wht-all)]
           [(e 1) (- (+ blk-all (* (sub1 e) blk-edg))
                     (+ wht-all (* (sub1 e) wht-edg)))]
           [(e c) (- (+ blk-all (* (sub1 e) blk-edg) (* (- c e) blk-cor))
                     (+ wht-all (* (sub1 e) wht-edg) (* (- c e) wht-cor)))]))])))

(check-expect ((type-count 5 5) new-game) 0)
(check-expect (piece-counting end-game) 60)


(: prefer-edges : Integer -> Heuristic)
;; Given a weight multipler for edge pieces, returns a Heuristic
;; which counts the number of black pieces with weight to edge pieces
(define (prefer-edges e)
  (type-count e 1))

(check-expect ((prefer-edges 3) new-game) 0)
(check-expect ((prefer-edges 3) end-game) 108)

(: prefer-edges-and-corners : Integer Integer -> Heuristic)
;; Given two weight multiplers for edge and corner pieces, returns a Heuristic
;; which counts the number of black pieces with weight to edge and corner pieces
(define (prefer-edges-and-corners e c)
  (type-count e c))

(check-expect ((prefer-edges-and-corners 5 5) new-game) 0)
(check-expect ((prefer-edges-and-corners 1 1) end-game) 60)

(: minimax-eval : Heuristic Integer Game -> Integer)
;; given a ply and heuristic, return the highest value out of all moves
(define (minimax-eval h ply g)
  (match g
    [(Game b pl)
     (local
       {(define pos-moves (ls-of-pos-moves all-pos g))}
       (cond
         [(game-over? g) (h g)]
         [(equal? '() pos-moves)
          (minimax-eval h ply  (Game b (other-player pl)))]
         [(= ply 0) (h g)]
         [else
          (match pl 
            ['white 
             (argmin (λ ([x : Integer]) x)
                     (map (λ ([po : Pos])
                            (minimax-eval h (- ply 1) (apply-move g pl po))) 
                          pos-moves))] 
            ['black 
             (argmax (λ ([x : Integer]) x)
                     (map (λ ([po : Pos])
                            (minimax-eval h (- ply 1) (apply-move g pl po))) 
                          pos-moves))])]))]))

(: minimax-history : Heuristic Integer Game (Listof Pos) -> (Pair Integer (Listof Pos)))
;; Returns a Pair of Integer (as calculated by heristic) and the history of moves to
;; get there
(define (minimax-history h ply g acc)
  (match g
    [(Game b pl)
     (local
       {(define pos-moves (ls-of-pos-moves all-pos g))}
       (cond
         [(game-over? g) (Pair (h g) acc)]
         [(equal? '() pos-moves)
          (minimax-history h ply  (Game b (other-player pl)) acc)]
         [(= ply 0) (Pair (h g) acc)]
         [else
          (match pl 
            ['white 
             ;(argmin (λ ([x : Pair]) (Pair-fst x)) 
             ;; argmin should be used
             ;; (first ...) is used instead of (argmin ...) because the compiler refuses
             ;; to apply it for unknown reason
             (first (map (λ ([po : Pos])
                           (minimax-history h (- ply 1) (apply-move g pl po) 
                                            (append (list po) acc))) 
                         pos-moves))] 
            ['black 
             ;(argmax (λ ([x : Pair]) (Pair-fst x))
             ;; same here
             (first  (map (λ ([po : Pos])
                            (minimax-history h (- ply 1) (apply-move g pl po) 
                                             (append (list po) acc))) 
                          pos-moves))])]))]))

(: minimax : Heuristic Integer -> Strategy)
;;
(define (minimax h ply)
  (lambda ([g : Game]) (first (Pair-snd (minimax-history h ply g '())))))


(: pick-upto : (All (a) Integer (Listof a) -> (Listof a)))
;; shuffle the list random and takes the first number of elements
(define (pick-upto n ls)
  (take (shuffle ls) n))

(check-expect (length (pick-upto 1 '(1 2 3 4))) 1)
(check-expect (length (pick-upto 3 '(1 2 3 4))) 3)


(: montymax : Heuristic Integer Integer -> Strategy)
;; identical to minimax except for having a maximum branching factor
;; which randomly selects a given number of branches to explore
(define (montymax h ply n)
  (lambda ([g : Game]) (first (Pair-snd (montymax-history h ply n g '())))))


(: montymax-history : Heuristic Integer Integer Game (Listof Pos) 
   -> (Pair Integer (Listof Pos)))
;; identical to minimax except for having a maximum branching factor
(define (montymax-history h ply n g acc)
  (match g
    [(Game b pl)
     (local
       {(define pos-moves (pick-upto n (ls-of-pos-moves all-pos g)))}
       (cond
         [(game-over? g) (Pair (h g) acc)]
         [(equal? '() pos-moves)
          (minimax-history h ply  (Game b (other-player pl)) acc)]
         [(= ply 0) (Pair (h g) acc)]
         [else
          (match pl 
            ['white 
             ;(argmin (λ ([x : Pair]) (Pair-fst x))
             ;; same error as minimax-history
             (first (map (λ ([po : Pos])
                           (minimax-history h (- ply 1) (apply-move g pl po) 
                                            (append (list po) acc))) 
                         pos-moves))] 
            ['black 
             ;(argmax (λ ([x : Pair]) (Pair-fst x))
             ;; same here
             (first  (map (λ ([po : Pos])
                            (minimax-history h (- ply 1) (apply-move g pl po) 
                                             (append (list po) acc))) 
                          pos-moves))])]))]))

(test)