#lang typed/racket/base

(require "assemble-structs.rkt"
         "collect-jump-targets.rkt"
         "../compiler/il-structs.rkt"
         "../compiler/expression-structs.rkt"
         racket/list)


;; Breaks up a sequence of statements into a list of basic blocks.
;;
;; The first basic block is special, and represents the start of execution.
;;
;; A basic block consists of a sequence of straight line statements, followed by one of
;; the following:
;;
;;     * A conditional jump.
;;     * An unconditional jump.
;;     * Termination.

(provide fracture)



;; fracture: (listof stmt) -> (listof basic-block)
(: fracture ((Listof Statement) -> (values (Listof BasicBlock)
                                           (Listof Symbol))))
(define (fracture stmts)
  (let*: ([first-block-label : Symbol (if (and (not (empty? stmts))
                                               (symbol? (first stmts)))
                                          (first stmts)
                                          (make-label 'start))]
          [stmts : (Listof Statement) (if (and (not (empty? stmts))
                                               (symbol? (first stmts)))
                                          (rest stmts)
                                          stmts)]
          [jump-targets : (Listof Symbol)
                        (cons first-block-label (collect-general-jump-targets stmts))]
          [entry-points : (Listof Symbol)
                        (cons first-block-label (collect-entry-points stmts))])
    (let: loop : (values (Listof BasicBlock) (Listof Symbol))
      ([name : Symbol first-block-label]
       [acc : (Listof UnlabeledStatement) '()]
       [basic-blocks  : (Listof BasicBlock) '()]
       [stmts : (Listof Statement) stmts]
       [last-stmt-goto? : Boolean #f])
      (cond
        [(null? stmts)
         (values (reverse (cons (make-BasicBlock name (reverse acc))
                                basic-blocks))
                 entry-points)]
        [else
         (let: ([first-stmt : Statement (car stmts)])
           (: do-on-label (Symbol -> (values (Listof BasicBlock) (Listof Symbol))))
           (define (do-on-label label-name)
             (cond
               [(member label-name jump-targets)
                (loop label-name
                      '()
                      (cons (make-BasicBlock 
                             name  
                             (if last-stmt-goto? 
                                 (reverse acc)
                                 (reverse (append `(,(make-GotoStatement (make-Label label-name)))
                                                  acc))))
                            basic-blocks)
                      (cdr stmts)
                      last-stmt-goto?)]
               [else
                (loop name
                      acc
                      basic-blocks
                      (cdr stmts)
                      last-stmt-goto?)]))
           (cond
             [(symbol? first-stmt)
              (do-on-label first-stmt)]
             [(LinkedLabel? first-stmt)
              (do-on-label (LinkedLabel-label first-stmt))]
             [else
              (loop name
                    (cons first-stmt acc)
                    basic-blocks
                    (cdr stmts)
                    (GotoStatement? (car stmts)))]))]))))