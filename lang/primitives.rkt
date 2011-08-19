#lang at-exp planet dyoo/whalesong

(provide min max)

(define (min x y)
    (if (< x y) x y))

(define (max x y)
  (if (> x y) x y))

