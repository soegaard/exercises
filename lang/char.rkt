#lang at-exp planet dyoo/whalesong

(provide char-whitespace?)

(define (char-whitespace? c)
  (or (eq? c #\space)
      (eq? c #\newline)
      (eq? c #\tab)))

      
