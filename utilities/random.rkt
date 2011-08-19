#lang at-exp planet dyoo/whalesong
(require (for-syntax racket))

(provide interval
         random-in-interval)

; interval : integer integer -> integer
;   return (list from from+1 ... to-1)
(define (interval from to)
  ; from inclusive, to exclusive
  (if (>= from to)
      '()
      (cons from (interval (+ from 1) to))))

; random-in-interval : integer integer -> integer
;   return a random integer x such that from <= x <= to
(define (random-in-interval from to)
  (+ from (random (+ (- to from) 1))))
