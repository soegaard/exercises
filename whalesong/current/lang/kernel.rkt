#lang racket/base
(require (prefix-in racket: (only-in racket/math pi sinh cosh sqr
                                     sgn conjugate))
         (prefix-in racket: racket/base)
	 racket/local
	 (for-syntax racket/base))



;; constants
(define true #t)
(define false #f)
(define pi racket:pi)
(define e (racket:exp 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive function stubs

;; provide-stub-function
(define-syntax (provide-stub-function stx)
  (syntax-case stx ()
    [(_ name-or-name-pair ...)
     (with-syntax ([(provided-name ...)
                    (map (lambda (name-or-pair)
                           (syntax-case name-or-pair ()
                             [x
                              (identifier? #'x)
                              #'x]
                             [(x y)
                              #'x]))
                         (syntax->list #'(name-or-name-pair ...)))]
                   [(impl-name ...) 
                    (map (lambda (name)
                           (syntax-case name ()
                             [an-id
                              (identifier? #'an-id)
                              (datum->syntax name 
                                             (string->symbol
                                              (string-append "racket:"
                                                             (symbol->string 
                                                              (syntax-e name))))
                                             name)]
                             [(an-id an-impl-name)
                              #'an-impl-name]))
                         (syntax->list #'(name-or-name-pair ...)))])
       (syntax/loc stx
         (begin (begin (define (provided-name . args) 
                         (racket:apply impl-name args))
                       (provide provided-name))
                ...)))]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provides
(provide true
         false
         pi
         e
         null
         #%plain-module-begin
	 #%module-begin
	 #%datum
	 #%app
	 #%top-interaction
	 #%top
         module
         define
	 define-values
         let-syntax
	 let-values
	 let*-values
	 define-struct
         struct
         if
	 cond
	 else
	 case
	 quote
	 quasiquote
	 unquote
	 unquote-splicing
	 lambda
	 case-lambda
	 let
	 let*
	 letrec
	 letrec-values
	 local
	 begin
	 begin0
	 set!
	 and
	 or
	 when
	 unless
	 require
	 for-syntax
         for-template
	 define-for-syntax
	 begin-for-syntax
	 prefix-in
	 only-in
	 provide
	 planet
	 all-defined-out
	 all-from-out
	 except-out
	 rename-out
	 struct-out
         define-syntax-rule
	 define-syntax
	 define-syntaxes


         let/cc
	 with-continuation-mark


         


         ;; Kernel inlinable
         *
	 -
	 +
	 =
	 /
	 sub1
	 add1
	 <
	 >
	 <=
	 >=
         cons
         car
         cdr
         list
         list?
         pair?
         null?
         not
         eq?
         values

         apply
         call-with-values)


(define (-identity x) x)

(define (-undefined? x)
  (letrec ([y y])
    (eq? x y)))



;; Many of these should be pushed upward rather than stubbed, so that
;; Racket's compiler can optimize these.
(provide-stub-function

 current-output-port
 current-print 
 write
 write-byte
 display
 newline
 displayln

 
;;  current-continuation-marks

;;  continuation-mark-set?
;;  continuation-mark-set->list

;;  make-struct-type
;;  make-struct-field-accessor
;;  make-struct-field-mutator
;;  struct-type?
;;  struct-constructor-procedure?
;;  struct-predicate-procedure?
;;  struct-accessor-procedure?
;;  struct-mutator-procedure?

;;  make-arity-at-least
;;  arity-at-least?
;;  arity-at-least-value


;;  compose
;;  current-inexact-milliseconds
;;  current-seconds
  void
  random
;;  sleep
;;  (identity -identity)
;;  raise
  
error
raise-type-error
raise-mismatch-error

;;  make-exn
;;  make-exn:fail
;;  make-exn:fail:contract
;;  make-exn:fail:contract:arity
;;  make-exn:fail:contract:variable
;;  make-exn:fail:contract:divide-by-zero

;;  exn-message
;;  exn-continuation-marks

;;  exn?
;;  exn:fail?
;;  exn:fail:contract?
;;  exn:fail:contract:arity?
;;  exn:fail:contract:variable?
;;  exn:fail:contract:divide-by-zero?
  abs
  quotient
  remainder
  modulo
;;  max
;;  min
  gcd
  lcm
  floor
  ceiling
  round
  truncate
  numerator
  denominator
  expt
  exp
  log
  sin
  sinh
  cos
  cosh
  tan
  asin
  acos
  atan
  sqr
  sqrt
  integer-sqrt
  sgn
  make-rectangular
  make-polar
  real-part
  imag-part
  angle
  magnitude
  conjugate
  ;;  inexact->exact
;;  exact->inexact
  number->string
  string->number
  procedure?
  procedure-arity
  procedure-arity-includes?
  procedure-rename
  ;;  (undefined? -undefined?)
;;  immutable?
;;  void?
symbol?
string?
char?
boolean?
vector?
;;  struct?
;;  eof-object?
;;  bytes?
;;  byte?
number?
;;  complex?
;;  real?
;;  rational?
integer?
exact?
exact-nonnegative-integer?
;;  inexact?
;;  odd?
;;  even?
  zero?
;;  positive?
;;  negative?
;;  box?
;;  hash?

  equal?
  eqv?

  caar
;;  cadr
;;  cdar
;;  cddr
;;  caaar
;;  caadr
;;  cadar
;;  cdaar
;;  cdadr
;;  cddar
;;  caddr
;;  cdddr
;;  cadddr
  length
  list*
  list-ref
;;  list-tail
  append
  reverse
  for-each
  map
;;  andmap
;;  ormap
memq
;;  memv
  member
;;  memf
  assq
;;  assv
;;  assoc
;;  remove
;;  filter
;;  foldl
;;  foldr
;;  sort
;;  build-list
  box
;;  box-immutable
  unbox
  set-box!
;;  make-hash
;;  make-hasheq
;;  hash-set!
;;  hash-ref
;;  hash-remove!
;;  hash-map
;;  hash-for-each
  make-string
  string
  string-length
  string-ref
  string=?
;;  string-ci=?
;;  string<?
;;  string>?
;;  string<=?
;;  string>=?
;;  string-ci<?
;;  string-ci>?
;;  string-ci<=?
;;  string-ci>=?
  substring
  string-append
  string->list
  list->string
;;  string-copy
string->symbol
symbol->string
  format
  printf
  fprintf
;;  build-string
;;  string->immutable-string
  string-set!
;;  string-fill!
;;  make-bytes
;;  bytes
;;  bytes->immutable-bytes
;;  bytes-length
;;  bytes-ref
;;  bytes-set!
;;  subbytes
;;  bytes-copy
;;  bytes-fill!
;;  bytes-append
;;  bytes->list
;;  list->bytes
;;  bytes=?
;;  bytes<?
;;  bytes>?
  make-vector
  vector
  vector-length
  vector-ref
  vector-set!
  vector->list
  list->vector
;;  build-vector
char=?
;;  char<?
;;  char>?
;;  char<=?
;;  char>=?
;;  char-ci=?
;;  char-ci<?
;;  char-ci>?
;;  char-ci<=?
;;  char-ci>=?
;;  char-alphabetic?
;;  char-numeric?
;;  char-whitespace?
;;  char-upper-case?
;;  char-lower-case?
;;  char->integer
;;  integer->char
;;  char-upcase
;;  char-downcase

 
;;  call-with-current-continuation
  call/cc
;;  call-with-continuation-prompt
;;  abort-current-continuation
;;  default-continuation-prompt-tag
;;  make-continuation-prompt-tag
;;  continuation-prompt-tag?

;;  make-reader-graph
;;  make-placeholder
;;  placeholder-set!
 )






(provide set-car! set-cdr!)

(define (set-car! x v)
  (error 'set-car! "Not available outside JavaScript context."))

(define (set-cdr! x v)
  (error 'set-car! "Not available outside JavaScript context."))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
