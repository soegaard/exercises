#lang at-exp planet dyoo/whalesong
(require (for-syntax racket))

(provide do)

(define-syntax do
  (lambda (orig-x)
    (syntax-case orig-x ()
      ((_ ((var init . step) ...) (e0 e1 ...) c ...)
       (with-syntax (((step ...)
                      (map (lambda (v s)
                             (syntax-case s ()
                               (() v)
                               ((e) (syntax e))
                               (_ (raise-syntax-error 
                                   #f
                                   "bad variable syntax"
                                   orig-x))))
                           (syntax->list (syntax (var ...)))
                           (syntax->list (syntax (step ...))))))
         (syntax/loc orig-x
           (let doloop ((var init) ...)
             (if e0
                 (begin (void) e1 ...)
                 (begin c ... (doloop step ...))))))))))
