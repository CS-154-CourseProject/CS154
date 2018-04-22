#lang racket
(provide (all-defined-out))
(require racket/vector)

(define size 30)
(define board 1)
(define n 30)
(define max-depth 2)

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))

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


(define (is-endgame? current-player board)
  (let* ((posns (append* (map (lambda (x) (map (lambda (y) (cons x y)) (range size))) (range size))))
         (filtered-posns-1
          (filter (lambda (posn)
                    (and
                     (player-posns? 2 (car posn) (cdr posn) 1)
                     (= (2d-vector-ref board (car posn) (cdr posn)) 1)))
                  posns))
         (filtered-posns-2
          (filter (lambda (posn)
                    (and
                     (player-posns? 1 (car posn) (cdr posn) 1)
                     (= (2d-vector-ref board (car posn) (cdr posn)) 2)))
                  posns)))
        (cond [(= 1 current-player) (= (length filtered-posns-1) 10)]
              [(= 2 current-player) (= (length filtered-posns-2) 10)])))
    ;(if (or (= (length filtered-posns-1) 10) (= (length filtered-posns-2) 10)) #t #f)))

(define (cprod l1 l2)
  (append* (map (lambda (y) (map (lambda (x) (cons y x)) l2)) l1)))
(define (part-board? coord)
  (part-of-board? (car coord) (cdr coord) board))
(define valid-slots (filter part-board? (cprod (range n) (range n))))

(define (occupied-slot? i j board)
  (> (2d-vector-ref board i j) 0))

(define (empty-slot? i j board)
  (= (2d-vector-ref board i j) 0))

(define (get-direction-functions i)
  (if (even? i) (list (cons sub1 sub1)
                      (cons sub1 identity)
                      (cons add1 identity)
                      (cons add1 sub1)
                      (cons identity sub1)
                      (cons identity add1))
                (list (cons sub1 identity)
                      (cons sub1 add1)
                      (cons add1 add1)
                      (cons add1 identity)
                      (cons identity sub1)
                      (cons identity add1))))


; Returns the list of neighbouring slots starting with the one on top-left in cw order
; Doesn't check if the slot is out-of-board
(define (next-neighbour i j)
  (map (lambda (x) (cons ((car x) i) ((cdr x) j))) (get-direction-functions i)))  

; Returns the list of slots one hop away starting with the one on top-left in cw order
; Doesn't check if the slot is out-of-board
(define (second-nearest-neighbour i j)
  (let* ([f1 (get-direction-functions i)]
         [f2 (get-direction-functions (+ i 1))])
  (map (lambda (x) (cons ((cadr x) ((caar x) i)) ((cddr x) ((cdar x) j)))) (zip f1 f2))))

(define (zip l1 l2)
  (if (null? l2) null (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (possible-hops i j board)
  (let* ([next-zip-hop (zip (next-neighbour i j) (second-nearest-neighbour i j))])
    (map (lambda (x) (cdr x)) (filter (lambda (x) (and (>= (cadr x) 0) (>= (cddr x) 0)
                                                   (< (cadr x) size) (< (cddr x) size)
                                                   (occupied-slot? (caar x) (cdar x) board)
                                                   (empty-slot? (cadr x) (cddr x) board))) next-zip-hop))))
                                                   
(define (next-move pos board current-player)
  (filter (lambda (x) (and (>= (caar x) 0) (>= (cdar x) 0) (< (caar x) size) (< (cdar x) size)
                           (empty-slot? (caar x) (cdar x) board)))
          (append (map list (next-neighbour (car pos) (cdr pos))) (walk-through-hop board pos (list pos) '()))))

(define (walk-through-hop board pos l path)
  (let* ([single-hop (filter (lambda (x) (not (member x l))) (possible-hops (car pos) (cdr pos) board))])
    (if (null? single-hop) '() (remove-duplicates (append (map (lambda (x) (cons x path)) single-hop) (append* (map (lambda (x) (walk-through-hop board x (append single-hop l) (cons x path))) single-hop))))))) 

 (define (get-opposite-player x)
  (if (= x 1) 2 1))

;; Evaluate Board Function
  
(define (evaluate-board board current-player board-type move)

 (define current-endgame (is-endgame? current-player board))
 (define other-endgame (is-endgame? (get-opposite-player current-player) board))

 (define (vertical-distance row)
    (cond [(= 1 current-player) (abs (- row 3))]
          [(= 2 current-player) (abs (- row 21))]))

; (define (game-progress line)
;   (define (helper row)
;     (define required-row (vector-ref board row))
;     (define (helper1 vec i sum)
;       (cond [(= i size) sum]
;           [else (cond [(part-of-board? row i board-type)
;                        (if (= (vector-ref vec i) current-player)
;                            (cond [(> (vertical-distance row) line) (helper1 vec (+ i 1) (+ sum 1))]
;                                  [else (helper1 vec (+ i 1) sum)])
;                            (helper1 vec (+ i 1) sum))]
;                       [else (helper1 vec (+ i 1) sum)])]))
;     (helper1 required-row 0 0))
;   
;   (/ (foldl (lambda (x y) (+ y (helper x))) 0 (range 0 size)) 10))
;
;  (define g (game-progress 5))

 (define (score-evaluater row column current-player board-type)
  
   (define wvertical 1)
   (define whop 0.75)
   (define wbackmove 0.75)
   
   (define (horizontal-distance)
     (let* ([centre 11]
            [score (- centre (abs (- centre column)))])
       (cond [(player-posns? current-player row column board-type) 0]
             [else score])))
   
   (define (is-edge? board-type)
     (cond [(= 2 current-player) (and  (> row 3)
                                       (< 8 row)
                                       (if (even? row) (or (= column (- 12 (/ row 2)))
                                                           (= column (+ 8 (/ row 2))))
                                           (or (= row (- 23 (* 2 column)))
                                               (= row (- 15 (* 2 column))))))]
           [(= 1 current-player) (and (< row 21) (> row 16)
                                      (if (even? row) (or (= column (/ row 2))
                                                          (= (* 2 column) (- 40 row)))
                                          (or (= column (quotient row 2))
                                              (= (* 2 column) (- 39 row)))))]))

   (define move-score1 (- (caadr move) (caar move)))
   (define move-score2 (- 18 (vertical-distance (caar move))))

   (let ([n-score (+ (* wvertical (vertical-distance row)) (horizontal-distance) (* whop move-score1) (* wbackmove move-score2))])
     (cond ;[(and (player-posns? current-player row column board-type) (<= g 1)) (/ -22 (vertical-distance row))]
           [(player-posns? (get-opposite-player current-player) row column board-type)
            (if (and (is-edge? board-type)) (+ n-score 3) n-score)]
           [else n-score])))

 (define (heuristic-helper row current-player) ;Takes a row and current player and returns the its evaluted score

   (define required-row (vector-ref board row))
   (define (helper vec i sum)
     (cond [(= i size) sum]
           [else (cond [(part-of-board? row i board-type)
                     (if (= (vector-ref vec i) current-player)
                         (helper vec (+ i 1) (+ sum (score-evaluater row i current-player board-type)))
                         (helper vec (+ i 1) sum))]
                       [else (helper vec (+ i 1) sum)])]))
   (helper required-row 0 0))
  
  (define opposite-player (get-opposite-player current-player))
  
  (define Total-self
    (foldl (lambda (x y) (+ y (heuristic-helper x current-player))) 0 (build-list size (lambda(x) x))))
  
  (define Total-opponent
    (foldl (lambda(x y) (+ y (heuristic-helper x opposite-player))) 0 (build-list size (lambda(x) x))))

  (cond [current-endgame 1000]
        [other-endgame -1000]
        [else (- Total-self Total-opponent)]))
  
;; Minimax Function

(define (minimax board is-maximising-player? current-player root-player depth board-type alpha beta move)
  
  (define best-val
    (cond [is-maximising-player? -inf.0]
          [else +inf.0]))

  (define (make-move board pos1 pos2)
    (let* ([i1 (car pos1)]
           [j1 (cdr pos1)]
           [i2 (car pos2)]
           [j2 (cdr pos2)]
           [peg (2d-vector-ref board i1 j1)]
           [board1 (for/vector ((i size)) (vector-copy (vector-ref board i)))])
      (2d-vector-set! board1 i1 j1 0)
      (2d-vector-set! board1 i2 j2 peg)
      board1))

  (define (compare val1 val2)
   (cond [is-maximising-player? (> (caddr val1) (caddr val2))]
         [else (< (caddr val1) (caddr val2))]))
                         
  (define (minimax-helper1 board pos next-move-list init alpha beta)
    (cond [(null? next-move-list) init]
          [else (let* ([next-pos (caar next-move-list)]
                       [new-board (make-move board pos next-pos)]
                       [opposite-player (get-opposite-player current-player)]
                       [top-move (if (= depth max-depth) (list pos next-pos) move)]
                       [val (minimax new-board (not is-maximising-player?) opposite-player root-player (- depth 1) board-type alpha beta top-move)]
                       [optVal (if (compare val init) val init)]
                       [alpha-new (if is-maximising-player? (max alpha (caddr optVal)) alpha)]
                       [beta-new (if (not is-maximising-player?) (min beta (caddr optVal)) beta)])
                (cond [(<= beta-new alpha-new) (if (compare val init) (list pos next-pos (caddr optVal)) init)]
                      [(compare optVal init) (minimax-helper1 board pos (cdr next-move-list) (list pos next-pos (caddr optVal)) alpha-new beta-new)]
                      [else (minimax-helper1 board pos (cdr next-move-list) init alpha-new beta-new)]))]))

  (define (minimax-helper2 board current-positions init alpha beta)
    (cond [(null? current-positions) init]
          [else (let* ([pos (car current-positions)]
                       [next-move-list (next-move pos board current-player)]
                       [val (minimax-helper1 board pos next-move-list init alpha beta)]
                       [optVal (if (compare val init) val init)]
                       [alpha-new (if is-maximising-player? (max alpha (caddr optVal)) alpha)]
                       [beta-new (if (not is-maximising-player?) (min beta  (caddr optVal)) beta)])
                  (if (<= beta-new alpha-new) optVal
                      (minimax-helper2 board (cdr current-positions) optVal alpha-new beta-new)))]))

  (let* ([current-positions (current-player-pegs board current-player board-type)]
         [init (list (cons 0 0) (cons 0 0) best-val)])
    (if (or (= depth 0) (is-endgame? 1 board) (is-endgame? 2 board))
               (list (cons 0 0) (cons 0 0) (evaluate-board board root-player board-type move))
               (minimax-helper2 board current-positions init alpha beta))))

;Returns a list of cons containing the positions of pegs of current-player
(define (current-player-pegs board current-player board-type) 
    (define (current-player-helper board row)
      (define required-row (vector-ref board row))
      (define (helper vec i acc)
        (cond [(= i size) acc]
              [else (if (= (vector-ref vec i) current-player)
                               (helper vec (+ i 1) (cons (cons row i) acc))
                               (helper vec (+ i 1) acc))]))
      (helper required-row 0 null))
    (foldl (lambda(x y) (append (current-player-helper board x) y)) null (build-list size (lambda(x) x))))