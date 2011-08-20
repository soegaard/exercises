#lang at-exp planet dyoo/whalesong
(require (for-syntax racket))
(require "../lang/do.rkt"
         "../lang/char.rkt")

(provide (all-defined-out))

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define (string-trim-left s)
  (if (string=? s "")
      s
      (do ([i 0 (+ i 1)])
        [(not (char-whitespace? (string-ref s i)))
         (substring s i (string-length s))])))

(define (string-trim-right s)
  (if (string=? s "")
      s
      (do ([i (string-length s) (- i 1)])
        [(not (char-whitespace? (string-ref s (- i 1))))
         (substring s 0 i)])))

(define (string-trim-both s)
  (string-trim-right
   (string-trim-left s)))


(define (string-append-n str n)
  (if (<= n 0)
      ""
      (let ([r (string-append-n str (- n 1))])
        (string-append str r))))
