#lang typed/racket/base

;; Does some basic optimizations at the level of basic blocks.


(require "assemble-structs.rkt"
         "../compiler/il-structs.rkt"
         racket/list)


(require/typed "../logger.rkt" [log-debug (String -> Void)])


(provide optimize-basic-blocks)


(define-type Blockht (HashTable Symbol BasicBlock))

;; We want maps from unlabeled statements to their respective blocks.
(define-type Bodyht (HashTable (Listof UnlabeledStatement) (Listof Symbol)))


(: optimize-basic-blocks ((Listof BasicBlock) -> (Listof BasicBlock)))
(define (optimize-basic-blocks blocks)

  (: blockht : Blockht)
  (define blockht (make-hasheq))

  (: bodyht : Bodyht)
  (define bodyht (make-hasheq))

  ;; First, scan through the blocks, and pick up their names and bodies.
  (for ([b blocks])
     (hash-set! blockht (BasicBlock-name b) b)

     (when (hash-has-key? bodyht (BasicBlock-stmts b))
       (log-debug (format "block ~a has the same content as another block" (BasicBlock-name b))))
     (hash-set! bodyht (BasicBlock-stmts b)
                (cons (BasicBlock-name b)
                      (hash-ref bodyht (BasicBlock-stmts b) (lambda () '())))))

  blocks
  #;(define inlined-blocks
    (map (lambda: ([b : BasicBlock])
                  (optimize-block b blockht))
         blocks))
  #;inlined-blocks)




(: optimize-block (BasicBlock Blockht -> BasicBlock))
;; Simple optimization: optimize away single-statement goto blocks with their
;; immediate contents.
(define (optimize-block b blocks)
  (define stmts (BasicBlock-stmts b))
  (cond
   [(= (length stmts) 1)
    (define first-stmt (first stmts))
    (cond
     [(GotoStatement? first-stmt)
      (define target (GotoStatement-target first-stmt)) 
      (cond
       [(Label? target)
        (log-debug (format "inlining basic block ~a" (BasicBlock-name b)))
        (optimize-block (make-BasicBlock (BasicBlock-name b)
                                         (BasicBlock-stmts
                                          (hash-ref blocks (Label-name target))))
                        blocks)]
       [else
        b])]
     [else
      b])]
   [else
    b]))
