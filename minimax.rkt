#lang racket
(provide (all-defined-out))
(define size 30)

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))


(define (part-of-board? i j board)
  #t)

(define (is-endgame? board current-player)
  #f)

             
;(2d-vector-set! vboard 0 3 1)
;(2d-vector-set! vboard 1 2 1)
;(2d-vector-set! vboard 1 3 2)
;(2d-vector-set! vboard 2 2 1)
;(2d-vector-set! vboard 2 3 1)
;(2d-vector-set! vboard 2 4 2)
;(2d-vector-set! vboard 3 1 0)
;(2d-vector-set! vboard 3 2 2)
;(2d-vector-set! vboard 3 3 0)
;(2d-vector-set! vboard 3 4 0)
;(2d-vector-set! vboard 4 1 1)
;(2d-vector-set! vboard 4 2 0)
;(2d-vector-set! vboard 4 3 1)
;(2d-vector-set! vboard 5 0 0)
;(2d-vector-set! vboard 5 1 2)
;(2d-vector-set! vboard 5 2 0)

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
  (filter (lambda (x) (and (>= (car x) 0) (>= (cdr x) 0) (< (car x) size) (< (cdr x) size)
                           (empty-slot? (car x) (cdr x) board)))
          (append (next-neighbour (car pos) (cdr pos)) (walk-through-hop board pos (list pos)))))

(define (walk-through-hop board pos l)
  (let* ([single-hop (filter (lambda (x) (not (member x l))) (possible-hops (car pos) (cdr pos) board))])
    (if (null? single-hop) '() (remove-duplicates (append single-hop (append* (map (lambda (x) (walk-through-hop board x (append single-hop l))) single-hop))))))) 
                           




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


(define (remove-duplicates l)
  (define (dup-remove-helper l ans)
    (cond [(null? l) ans]
          [(member (car l) ans) (dup-remove-helper (cdr l) ans)]
          [else (dup-remove-helper (cdr l) (append ans (list (car l))))]))
  (dup-remove-helper l '()))