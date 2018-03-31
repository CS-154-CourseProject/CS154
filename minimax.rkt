#lang racket

;(require "pro.rkt")

(define size 5)

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))

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

(define (next-move pos board current-player)
  null)

(define (is-endgame? board)
  (let* ((posns (append* (map (lambda (x) (map (lambda (y) (cons x y)) (range size))) (range size))))
         (filtered-posns-1 (filter (lambda (posn) (and (player-quadrant (car posn) (cdr posn) 2) (= (2d-vector-ref board (car posn) (cdr posn)) 1))) posns))
         (filtered-posns-2 (filter(lambda (posn) (and (player-quadrant (car posn) (cdr posn) 1) (= (2d-vector-ref board (car posn) (cdr posn)) 2))) posns)))
    (if (or (= (length filtered-posns-1) 10) (= (length filtered-posns-2) 10)) #t #f)))

(define board (make-2d-vector size size 0))
(2d-vector-set! board 0 2 1)
(2d-vector-set! board 1 1 1)
(2d-vector-set! board 1 2 1)
(2d-vector-set! board 2 1 1)
(2d-vector-set! board 2 2 1)
(2d-vector-set! board 3 2 2)
(2d-vector-set! board 3 1 2)
(2d-vector-set! board 4 2 2)

;; Evaluate Board Function
  
(define (evaluate-board board current-player)

 (define (get-opposite-player x)
  (if (= x 1) 2 1))
  
 (define (vertical-distance row column current-player)
  (cond [(= 1 current-player) row]
        [(= 2 current-player) (- (- size 1) row)]))
  
 (define (heuristic-helper row current-player)
   (define required-row (vector-ref board row))
   (define (helper vec i sum)
     (cond [(= i size) sum]
           [else (cond [(part-of-board? row i board)
                     (if (= (vector-ref vec i) current-player)
                         (helper vec (+ i 1) (+ sum (vertical-distance row i current-player)))
                         (helper vec (+ i 1) sum))]
                       [else (helper vec (+ i 1) sum)])]))
   (helper required-row 0 0))
  
  (define opposite-player (get-opposite-player current-player))
  
  (define Total-self
    (foldl (lambda(x y) (+ y (heuristic-helper x current-player))) 0 (build-list size (lambda(x) x))))
  
  (define Total-opponent
    (foldl (lambda(x y) (+ y (heuristic-helper x opposite-player))) 0 (build-list size (lambda(x) x))))

  (- Total-self Total-opponent))

;; Minimax Function

(define (minimax board current-player depth)

  (define (get-opposite-player x)
  (if (= x 1) 2 1))
  
  (define (make-move board pos1 pos2)
    (let* ([i1 (car pos1)]
           [j1 (cdr pos1)]
           [i2 (car pos2)]
           [j2 (cdr pos2)]
           [peg (2d-vector-ref board i1 j1)])
      (2d-vector-set! board i1 j1 0)
      (2d-vector-set! board i2 j2 peg)
      board))
  
  (define (current-player-pegs board)
    (define (current-player-helper board row)
      (define required-row (vector-ref board row))
      (define (helper vec i acc)
        (cond [(= i size) acc]
              [else (cond [(part-of-board? row i board)
                           (if (= (vector-ref vec i) current-player)
                               (helper (+ i 1) (cons (cons row i) acc))
                               (helper (+ i 1) acc))]
                          [else (helper (+ i 1) acc)])]))
      (helper required-row 0 null))
    (foldl (lambda(x y) (append (current-player-helper board x) y)) null (build-list size (lambda(x) x))))

  (define best-val
   (cond [(= 1 current-player) -inf.0]
         [(= 2 current-player) +inf.0]))

  (define (compare val1 val2)
   (cond [(= 1 current-player) (> (caddr val1) (caddr val2))]
         [(= 2 current-player) (< (caddr val1) (caddr val2))]))
                         
  (define (minimax-helper1 board pos next-move-list init)
    (cond [(null? next-move-list) init]
          [else (let* ([next-pos (car next-move-list)]
                      [new-board (make-move board pos next-pos)]
                      [val (minimax new-board (get-opposite-player current-player) (- depth 1))])
                (if (compare val init)
                    (minimax-helper1 pos (cdr next-move-list) (list pos next-pos (caddr val)))
                    (minimax-helper1 pos (cdr next-move-list) init)))]))

  (define (minimax-helper2 board current-positions init)
    (cond [(null? current-positions) init]
          [else (let* ([pos (car current-positions)]
                       [next-move-list (next-move pos board current-player)]
                       [val (minimax-helper1 board pos next-move-list init)])
                  (if (compare val init)
                      (minimax-helper2 board (cdr current-positions) val)
                      (minimax-helper2 board (cdr current-positions) init)))]))

  (let* ([current-positions (current-player-pegs board)]
         [init (list (cons 0 0) (cons 0 0) best-val)])
    (if (or (= depth 0) (is-endgame? board))
               (list (cons 0 0) (cons 0 0) (evaluate-board board current-player))
               (minimax-helper2 board current-positions init))))
         