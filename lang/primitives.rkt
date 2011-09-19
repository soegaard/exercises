#lang at-exp planet dyoo/whalesong

(provide min max cadr caddr)

(define (min x y)
    (if (< x y) x y))

(define (max x y)
  (if (> x y) x y))

(define (cadr x)
  (car (cdr x)))

(define (caddr x)
  (car (cdr (cdr x))))
