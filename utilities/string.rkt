#lang at-exp planet dyoo/whalesong
(require (for-syntax racket))

(provide (all-defined-out))

;;; String Utilities

#;(define (string->list s)
    (let ([len (string-length s)])
      (let loop ([i 0] [l '()])
        (if (= i len) 
            (reverse l)
            (loop (+ i 1) (cons (string-ref s i) l))))))

#;(define (string->list s)
  (let ([len (string-length s)])
    (do ([i 0 (+ i 1)]
         [l '() (cons (string-ref s i) l)])
      [(= i len) (reverse l)])))


#;(define (string-reverse s)
    (list->string (reverse (string->list s))))

#;(define (string-trim-left s)
    (for/first ([c (in-string s)]
                [i (in-naturals)]
                #:when (not (char-whitespace? c)))
      (substring s i (string-length s))))

#;(define (string-trim-right s)
  (for/first ([c (in-string (string-reverse s))]
              [i (in-naturals)]
              #:when (not (char-whitespace? c)))
             (substring s 0 (- (string-length s) i))))

#;(define (string-trim-both s)
    (string-trim-left
     (string-trim-right s)))

(define (string-append-n str n)
  (if (<= n 0)
      ""
      (let ([r (string-append-n str (- n 1))])
        (string-append str r))))
