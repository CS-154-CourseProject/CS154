#lang racket
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

(provide (all-defined-out))

(define unit-size (* (sqrt 2) 21))
(define n 30)
(define board 3)
(define unit1
  (cond
   [(= board 1) (overlay (circle 9 "solid" "gray") (rhombus unit-size 60 "solid" "white"))]
   [(= board 2) (rhombus (- unit-size 0.4) 90 "solid" "brown")]
   [(= board 3) (overlay (circle (- (/ unit-size 2) 1) "outline" "black") (rhombus unit-size 90 "solid" "white"))]))

(define unit2
  (cond
   [(= board 1) (overlay (circle 9 "solid" "gray") (rhombus unit-size 60 "solid" "white"))]
   [(= board 2) (rhombus (- unit-size 0.4) 90 "solid" "LightGoldenrodYellow")]
   [(= board 3) (overlay (circle (- (/ unit-size 2) 1) "outline" "black") (rhombus unit-size 90 "solid" "white"))]))




(define size 1000)
(define l (make-list n unit1))

(define posns (map (lambda (x) (make-posn (* (image-width unit1) (+ x (/ 1 2))) (/ unit-size (sqrt 2)))) (range n)))


(define (get-single-row unit)
  (place-images (make-list n unit) posns (rectangle size (image-width unit1) "solid" "transparent")))

(define row-couple (underlay/xy (get-single-row unit1) (/ (image-width unit1) 2) (/ (image-height unit1) 2) (get-single-row unit2)))

(define size-x (image-width row-couple))
(define size-y (image-height row-couple))


(define posns_board (map (lambda (x) (make-posn (/ size-x 2) (+ (/ size-y 2) (* x (image-height unit1))))) (range n)))

(define full-board (place-images (make-list n row-couple) posns_board (rectangle size size "solid" "transparent")))


(define (index->coords x y)
  (if (= 0 (modulo x 2)) (cons (* (/ (image-width unit1) 2) (+ 1 (* 2 y))) (* (/ (image-height unit1) 2) (+ 1 (* 2 (quotient x 2)))))
      (cons (* (/ (image-width unit1) 2) (+ 2 (* 2 y))) (* (/ (image-height unit1) 2) (+ 2 (* 2 (quotient x 2)))))))

(define (ind->posns cords)
  (let* ([ans (index->coords (car cords) (cdr cords))])
    (make-posn (car ans) (cdr ans))))

(define peg (rhombus unit-size 90 "solid" "white"))

(define (place-peg image peg i j)
  (let* ([coords (index->coords i j)])
    (place-image peg (car coords) (cdr coords) image)))

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
                                              

(define (not-in-board? coord)
  (not (part-of-board? (car coord) (cdr coord) board)))

(define (part-board? coord)
  (part-of-board? (car coord) (cdr coord)))

(define (cprod l1 l2)
  (append* (map (lambda (y) (map (lambda (x) (cons y x)) l2)) l1)))
(define empty-slots (filter not-in-board? (cprod (range n) (range n))))

(define new-board (place-images (make-list (length empty-slots) peg) (map ind->posns empty-slots) full-board))

