#lang racket
(require "minimax.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

(define unit-size (* (sqrt 2) 21))
(define n 30)
(define n-players 2)
(define player-colors (list 'red 'green 'orange 'blue 'black))
(define player-next-colors (list 'LightPink 'GreenYellow 'Gold 'SkyBlue 'DarkGray))
(define pegs-per-player 10)
(define slot-radius 9)
(define board 1)
(define theta-of-unit (if (or (= board 2) (= board 3)) 90 60)) 
                          
(define unit1
  (cond
   [(= board 1) (overlay (circle slot-radius "solid" "gray") (rhombus unit-size theta-of-unit "solid" "white"))]
   [(= board 2) (rhombus (- unit-size 0.4) theta-of-unit "solid" "brown")]
   [(= board 3) (overlay (circle (- (/ unit-size 2) 1) "outline" "black") (rhombus unit-size theta-of-unit "solid" "white"))]))

(define unit2
  (cond
   [(= board 1) (overlay (circle slot-radius "solid" "gray") (rhombus unit-size theta-of-unit "solid" "white"))]
   [(= board 2) (rhombus (- unit-size 0.4) theta-of-unit "solid" "LightGoldenrodYellow")]
   [(= board 3) (overlay (circle (- (/ unit-size 2) 1) "outline" "black") (rhombus unit-size theta-of-unit "solid" "white"))]))

(define x-size 800)
(define y-size 600)
(define l (make-list n unit1))

(define posns (map (lambda (x) (make-posn (* (image-width unit1) (+ x (/ 1 2))) (/ (image-height unit1) 2))) (range n)))


(define (get-single-row unit)
  (place-images (make-list n unit) posns (rectangle x-size (image-height unit1) "solid" "transparent")))

(define row-couple (underlay/xy (get-single-row unit1) (/ (image-width unit1) 2) (/ (image-height unit1) 2) (get-single-row unit2)))

(define size-x (image-width row-couple))
(define size-y (image-height row-couple))


(define posns_board (map (lambda (x) (make-posn (/ size-x 2) (+ (/ size-y 2) (* x (image-height unit1))))) (range n)))

(define full-board (place-images (make-list n row-couple) posns_board (rectangle x-size y-size "solid" "transparent")))


(define (index->coords x y)
  (if (= 0 (modulo x 2)) (cons (* (/ (image-width unit1) 2) (+ 1 (* 2 y))) (* (/ (image-height unit1) 2) (+ 1 (* 2 (quotient x 2)))))
      (cons (* (/ (image-width unit1) 2) (+ 2 (* 2 y))) (* (/ (image-height unit1) 2) (+ 2 (* 2 (quotient x 2)))))))

(define (ind->posns cords)
  (let* ([ans (index->coords (car cords) (cdr cords))])
    (make-posn (car ans) (cdr ans))))

(define peg (rhombus unit-size 90 "solid" "white"))

(define (peg-for-player player board)
   (circle slot-radius "solid" (list-ref player-colors (- player 1))))

(define (next-pegs-player player board)
   (circle slot-radius "solid" (list-ref player-next-colors (- player 1))))

; To be moved to board-shape.rkt
(define (part-of-board? i j board)
  (cond
  [(or (= board 1) (= board 3)) (or (and (< i 17) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))
      (and (< i 21) (> i 7) (if (= 0 (modulo i 2)) (and (>= j (/ i 2)) (<= (* 2 j) (- 40 i)))
                                (and (>= j (quotient i 2)) (<= (* 2 j) (- 39 i))))))]
  [(= board 2) (or (and (< i 12) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))
                   (and (> i 11) (< i 19) (if (= 0 (modulo i 2)) (and (>= (* 2 j) (+ i 2)) (<= (* 2 j) (- 38 i)))
                                              (and (>= (* 2 j) (+ i 1)) (<= (* 2 j) (- 37 i))))))]))

; To be moved to board-shape.rkt
(define (player-posns? player i j board)
  (cond [(or (= board 1) (= board 3))
         (cond [(= player 1) (and (< i 8) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))]
               [(= player 2)  (and (< i 21) (> i 16) (if (= 0 (modulo i 2)) (and (>= j (/ i 2)) (<= (* 2 j) (- 40 i)))
                                (and (>= j (quotient i 2)) (<= (* 2 j) (- 39 i)))))])]
        [(= board 2)
         (cond [(= player 1) (and (< i 8) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))]
               [(= player 2) (and (> i 14) (< i 19) (if (= 0 (modulo i 2)) (and (>= (* 2 j) (+ i 2)) (<= (* 2 j) (- 38 i)))
                                              (and (>= (* 2 j) (+ i 1)) (<= (* 2 j) (- 37 i)))))])]))

(define (not-in-board? coord)
  (not (part-of-board? (car coord) (cdr coord) board)))

(define (part-board? coord)
  (part-of-board? (car coord) (cdr coord) board))

(define (cprod l1 l2)
  (append* (map (lambda (y) (map (lambda (x) (cons y x)) l2)) l1)))
(define empty-slots (filter not-in-board? (cprod (range n) (range n))))


(define empty-board (place-images (make-list (length empty-slots) peg) (map ind->posns empty-slots) full-board))

(define (place-initial-pegs i in-board)
  (if (= i 0) in-board
      (place-initial-pegs (- i 1)
                          (place-images (make-list pegs-per-player (peg-for-player i board))
                                        (map ind->posns (filter (lambda (x) (player-posns? i (car x) (cdr x) board)) (cprod (range n) (range n)))) in-board))))





(define initial-board (place-initial-pegs n-players empty-board))


; Player1: Computer (AI)
; Player2: User
; Player2 starts the game
; State5: Player2's turn
; State6: If player2 has clicked : button down on his peg -> state7 display nextpossible moves
; State7: if drag, move peg with mouse: if button-up: if valid next possible slot put it there >  state8, else place it in orig pos > state5
; State8: Game Over-> state15, else state10
; State10: AI's Turn : get best move from minimax, with time move/animate from current pos to 

(define peg-removed initial-board)
(define current-board initial-board)
(define prev-config initial-board)
(define next-list '())

(define (remove-peg curr-board i j)
  (let* ([coords (index->coords i j)])
    (place-image (if (even? i) unit1 unit2) (car coords) (cdr coords) curr-board)))

(define (inside-a-slot? x y i j board)
  (let* ([center (index->coords i j)]
         [xc (car center)]
         [yc (cdr center)])
    (cond [(or (= board 1) (= board 3)) (< (+ (expt (- x xc) 2) (expt (- y yc) 2)) (* slot-radius slot-radius))]
          [(= board 2) (let* ([cs (cos (/ theta-of-unit 2))]
                              [cot (/ 1 (tan (/ theta-of-unit 2)))])
                         (and (< (- y yc (* unit-size cs)) (- (* cot (- x xc))))
                              (< (- y yc (* unit-size cs)) (* cot (- x xc)))
                              (> (+ (- y yc) (* unit-size cs)) (- (* cot (- x xc))))
                              (> (+ (- y yc) (* unit-size cs)) (* cot (- x xc)))))])))

(define (place-peg image player i j ind)
  (let* ([coords (if ind (index->coords i j) (cons i j))])
    (place-image (peg-for-player player board) (car coords) (cdr coords) image)))

(define valid-slots (filter part-board? (cprod (range n) (range n))))

(define (get-index-of-clicked x y b)
  (filter (lambda (z) (inside-a-slot? x y (car z) (cdr z) b)) valid-slots))
  
;; Vector
(define vboard (make-2d-vector size size -1))
(map (lambda (x) (2d-vector-set! vboard (car x) (cdr x) 0)) valid-slots)
(define (fill-vector-posns i)
  (if (= i 0) vboard
      (begin (map (lambda (x) (2d-vector-set! vboard (car x) (cdr x) i))
                              (filter (lambda (x) (player-posns? i (car x) (cdr x) board)) (cprod (range n) (range n))))
             (fill-vector-posns (- i 1)))))
(fill-vector-posns 2)


(define initial (cons 0 0))

(define (create-scene state)
  (cond [(= state 5) current-board]
        ;Replace by result of minimax
        [(= state 10) 
                        current-board]
        [(= state 11) (text "Game Over" 36 "indigo")]
        [else current-board]))

(define (handle-mouse-events state x y event)
  (cond [(mouse=? event "button-down") (handle-button-down state x y)]
        [else state]))

(define (get-random-ai-move current-player)
  (let* [(next-moves (append* (map (lambda (x) (map (lambda (y) (list x y)) (next-move x vboard 1)))
                                   (current-player-pegs vboard current-player))))]
    (list-ref next-moves (random (length next-moves)))))



(define current-player 2)
(define (handle-button-down state x y)
  (cond [(= state 5)
          (let* ([ind (get-index-of-clicked x y board)])
            (if (and (not (null? ind)) (= 2 (2d-vector-ref vboard (caar ind) (cdar ind))))
                (begin (set! prev-config current-board)
                       (let* ([next (next-move (car ind) vboard current-player)])
                         (set! initial (car ind))
                         (set! peg-removed (remove-peg current-board (caar ind) (cdar ind)))
                         (set! current-board (place-images (make-list (length next) (next-pegs-player current-player board)) (map ind->posns next) current-board))
                         (set! next-list next)) 6) 5))]
        [(= state 6) 
         (let* ([ind (get-index-of-clicked x y board)])
           (cond [(and (not (null? ind)) (member (car ind) next-list))
                  (begin (set! current-board (place-peg peg-removed current-player (caar ind) (cdar ind) #t))
                         (2d-vector-set! vboard (car initial) (cdr initial) 0)
                         (2d-vector-set! vboard (caar ind) (cdar ind) current-player)
                         (set! current-player 1)
                         (if (is-endgame? vboard) 11 10))]
                 [(and (not (null? ind)) (= (2d-vector-ref vboard (caar ind) (cdar ind)) current-player)) (begin (set! current-board prev-config)
                                                                                                                 (handle-button-down 5 x y))]
                 [else state]))]
        [(= state 10) (let* ([ind (get-random-ai-move current-player)])
                        (begin 
                        (set! peg-removed (remove-peg current-board (caar ind) (cdar ind)))
                        (set! current-board (place-peg peg-removed current-player (caadr ind) (cdadr ind) #t))
                        (2d-vector-set! vboard (caar ind) (cdar ind) 0)
                        (2d-vector-set! vboard (caadr ind) (cdadr ind) current-player)
                        (set! current-player 2)(if (is-endgame? vboard) 11 5)))]
         [else state]))

; State: (state_number time i j start-i start-j)
(big-bang 5
          (on-tick identity)
          (on-mouse handle-mouse-events) 
          (to-draw create-scene))
