#lang s-exp "../lang/base.rkt"

(require "impl.rkt"
         "helpers.rkt"
         "event.rkt"
         (for-syntax racket/base))

(require (for-syntax racket/base racket/stxparam-exptime)
         (only-in "../lang/kernel.rkt" define-syntax-parameter syntax-parameterize))

(provide (except-out (all-from-out "impl.rkt")
                     big-bang
                     initial-view
                     stop-when
                     on-tick
                     on-mock-location-change
                     on-location-change
                     to-draw)
         (all-from-out "helpers.rkt")
         (all-from-out "event.rkt"))

(provide view-bind-many
         view-bind-many*
         view-prepend-child)


(provide (rename-out [internal-big-bang big-bang]
                     [big-bang big-bang/f]

                     
                     [initial-view initial-view/f]
                     [stop-when stop-when/f]

                     [on-tick on-tick/f]

                     [on-mock-location-change on-mock-location-change/f]

                     [on-location-change on-location-change/f]

                     [to-draw to-draw/f]))

(define-syntax-parameter in-big-bang? #f)

(define-syntax (internal-big-bang stx)
  (syntax-case stx ()
    [(_ body ...)
     (syntax/loc stx (big-bang (syntax-parameterize ([in-big-bang? #t])
                                                    body)
                               ...))]
    [else
     (raise-syntax-error #f "big-bang should be applied")]))

(define-syntax (define/provide-protected stx)
  (syntax-case stx ()
    [(_ (real-function ...))
     (with-syntax ([(internal-name ...) 
                    (generate-temporaries (syntax->list #'(real-function ...)))])
       (syntax/loc stx       
         (begin (begin (define-syntax (internal-name stx2)
                         (syntax-case stx2 ()
                           [(_ args (... ...))
                            (cond
                              [(syntax-parameter-value #'in-big-bang?)
                               
                               (syntax/loc stx2
                                 (real-function args (... ...)))]
                              [else
                               (raise-syntax-error #f (format "~a should be applied in the context of a big-bang"
                                                              'real-function)
                                                   stx2)])]
                           [else
                            (raise-syntax-error #f 
                                                (format "~a should be applied in the context of a big-bang"
                                                        'real-function)
                                                stx2)]))
                       (provide (rename-out (internal-name real-function)))) ...)))]))

(define/provide-protected (initial-view
                           stop-when
                           on-tick
                           on-mock-location-change
                           on-location-change
                           to-draw))
  


;; A syntactic form to make it more convenient to focus and bind multiple things
;; (view-bind-many a-view
;;             [id type function]
;;             [id type function] ...)
(define-syntax (view-bind-many stx)
  (syntax-case stx ()
    [(_ a-view [a-selector a-type a-function] ...)
     (foldl (lambda (a-selector a-type a-function a-view-stx)
              #'(view-bind (view-focus #,a-view-stx #,a-selector)
                           #,a-type
                           #,a-function))
            #'a-view
            (syntax->list #'(a-selector ...))
            (syntax->list #'(a-type ...))
            (syntax->list #'(a-function ...)))]))


;; We also provide a function to do the same thing, just in case.
(define (view-bind-many* a-view listof-id+type+function)

  (define (string-or-symbol? x)
    (or (string? x)
        (symbol? x)))

  (unless (list? listof-id+type+function)
    (raise-type-error 'view-bind-many*
                      "(listof (list id-string event-type-string world-updater))"
                      listof-id+type+function))
  (foldl (lambda (id+type+function a-view)
           (unless (and (list? id+type+function)
                        (string-or-symbol? (first id+type+function))
                        (string-or-symbol? (second id+type+function))
                        (procedure? (third id+type+function)))
             (raise-type-error 'view-bind-many*
                               "(list id-string event-type-string world-updater)"
                               id+type+function))
           (view-bind (view-focus a-view (first id+type+function))
                      (second id+type+function)
                      (third id+type+function)))
         a-view
         listof-id+type+function))



(define (view-prepend-child a-view c)
  (unless (view? a-view)
    (raise-type-error 'view-prepend-child
                      "view"
                      a-view))
  (cond
   [(view-down? a-view)
    (view-insert-left (view-down a-view) c)]
   [else
    (view-append-child a-view c)]))