#lang s-exp "../../lang/js/js.rkt"

(require "../structs.rkt")

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-impl.js")
 #:provided-values (specialize!))
