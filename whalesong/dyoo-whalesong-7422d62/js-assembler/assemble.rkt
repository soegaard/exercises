#lang typed/racket/base


;; Assembles the statement stream into JavaScript.


(require "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-expression.rkt"
         "assemble-perform-statement.rkt"
         "optimize-basic-blocks.rkt"
         "fracture.rkt"
         "../compiler/il-structs.rkt"
         "../sets.rkt"
         "../helpers.rkt"
         racket/string
         racket/list)
(require/typed "../logger.rkt"
               [log-debug (String -> Void)])

(provide assemble/write-invoke
         assemble-statement)





;; Parameter that controls the generation of a trace.
(define current-emit-debug-trace? (make-parameter #f))


;; Represents a hashtable from symbols to basic blocks
(define-type Blockht (HashTable Symbol BasicBlock))



(: assemble/write-invoke ((Listof Statement) Output-Port -> Void))
;; Writes out the JavaScript code that represents the anonymous invocation expression.
;; What's emitted is a function expression that, when invoked, runs the
;; statements.
(define (assemble/write-invoke stmts op)
  (fprintf op "(function(MACHINE, success, fail, params) {\n")
  (fprintf op "var param;\n")
  (fprintf op "var RUNTIME = plt.runtime;\n")
  
  (define-values (basic-blocks entry-points) (fracture stmts))
  
  (write-blocks basic-blocks (list->set entry-points) op)
  
  (write-linked-label-attributes stmts op)
  
  (fprintf op "MACHINE.params.currentErrorHandler = fail;\n")
  (fprintf op "MACHINE.params.currentSuccessHandler = success;\n")
  (fprintf op #<<EOF
for (param in params) {
    if (params.hasOwnProperty(param)) {
        MACHINE.params[param] = params[param];
    }
}
EOF
           )
  (fprintf op "MACHINE.trampoline(~a); })"
           (assemble-label (make-Label (BasicBlock-name (first basic-blocks))))))



(: write-blocks ((Listof BasicBlock) (Setof Symbol) Output-Port -> Void))
;; Write out all the basic blocks associated to an entry point.
(define (write-blocks blocks entry-points op)  
  (: blockht : Blockht)
  (define blockht (make-hash))
  
  (for ([b blocks])
    (hash-set! blockht (BasicBlock-name b) b))
  
  ;; Since there may be cycles between the blocks, we cut the cycles by
  ;; making them entry points as well.
  (insert-cycles-as-entry-points! entry-points blockht)

  (set-for-each (lambda: ([s : Symbol])
                  (log-debug (format "Emitting code for basic block ~s" s))
                  (displayln (assemble-basic-block (hash-ref blockht s) 
                                                   blockht
                                                   entry-points)
                             op)
                  (newline op))
                entry-points))



(: insert-cycles-as-entry-points! ((Setof Symbol) Blockht -> 'ok))
(define (insert-cycles-as-entry-points! entry-points blockht)
  (define visited ((inst new-seteq Symbol)))
  
  (: loop ((Listof Symbol) -> 'ok))
  (define (loop queue)
    (cond
      [(empty? queue)
       'ok]
      [else
       ;; Visit the next one.
       (define next-to-visit (first queue))
       (cond
         [(set-contains? visited next-to-visit)
          (unless (set-contains? entry-points next-to-visit)
            (log-debug (format "Promoting ~a to an entry point" next-to-visit))
            (set-insert! entry-points next-to-visit))
          (loop (rest queue))]
         [else
          (set-insert! visited next-to-visit)
          (loop (list-union (basic-block-out-edges (hash-ref blockht next-to-visit))
                            (rest queue)))])]))

  (loop (set->list entry-points)))





(: write-linked-label-attributes ((Listof Statement) Output-Port -> 'ok))
(define (write-linked-label-attributes stmts op)
  (cond
    [(empty? stmts)
     'ok]
    [else
     (let: ([stmt : Statement (first stmts)])
       
       (define (next) (write-linked-label-attributes (rest stmts) op))
       
       (cond
         [(symbol? stmt)
          (next)]
         [(LinkedLabel? stmt)
          (fprintf op "~a.multipleValueReturn = ~a;\n" 
                   (assemble-label (make-Label (LinkedLabel-label stmt)))
                   (assemble-label (make-Label (LinkedLabel-linked-to stmt))))
          (next)]
         [(DebugPrint? stmt)
          (next)]
         [(AssignImmediateStatement? stmt)
          (next)]
         [(AssignPrimOpStatement? stmt)
          (next)]
         [(PerformStatement? stmt)
          (next)]
         [(TestAndJumpStatement? stmt)
          (next)]
         [(GotoStatement? stmt)
          (next)]
         [(PushEnvironment? stmt)
          (next)]
         [(PopEnvironment? stmt)
          (next)]
         [(PushImmediateOntoEnvironment? stmt)
          (next)]
         [(PushControlFrame/Generic? stmt)
          (next)]
         [(PushControlFrame/Call? stmt)
          (next)]
         [(PushControlFrame/Prompt? stmt)
          (next)]
         [(PopControlFrame? stmt)
          (next)]
         [(Comment? stmt)
          (next)]))]))





;; assemble-basic-block: basic-block -> string
(: assemble-basic-block (BasicBlock Blockht (Setof Symbol) -> String))
(define (assemble-basic-block a-basic-block blockht entry-points)
  (format "var ~a = function(MACHINE){
    if(--MACHINE.callsBeforeTrampoline < 0) {
        throw ~a;
    }
    ~a
};"
          (assemble-label (make-Label (BasicBlock-name a-basic-block)))
          (assemble-label (make-Label (BasicBlock-name a-basic-block)))
          (string-join (assemble-block-statements (BasicBlock-name a-basic-block)
                                                  (BasicBlock-stmts a-basic-block)
                                                  blockht
                                                  entry-points)
                       "\n")))


(: assemble-block-statements (Symbol (Listof UnlabeledStatement) Blockht (Setof Symbol) -> (Listof String)))
(define (assemble-block-statements name stmts blockht entry-points)
  
  (: default (UnlabeledStatement -> (Listof String)))
  (define (default stmt)
    (when (and (empty? (rest stmts))
               (not (GotoStatement? stmt)))
      (log-debug (format "Last statement of the block ~a is not a goto" name)))

    (cons (assemble-statement stmt)
          (assemble-block-statements name
                                     (rest stmts) 
                                     blockht
                                     entry-points)))
  
  (cond [(empty? stmts)
         empty]
        [else
         (define stmt (first stmts))
         (cond
           [(DebugPrint? stmt)
            (default stmt)]
           
           [(AssignImmediateStatement? stmt)
            (default stmt)]
           
           [(AssignPrimOpStatement? stmt)
            (default stmt)]
           
           [(PerformStatement? stmt)
            (default stmt)]
           
           [(TestAndJumpStatement? stmt)
            (define test (TestAndJumpStatement-op stmt))
            
            (: test-code String)
            (define test-code (cond
                                [(TestFalse? test)
                                 (format "if (~a === false)"
                                         (assemble-oparg (TestFalse-operand test)))]
                                [(TestTrue? test)
                                 (format "if (~a !== false)"
                                         (assemble-oparg (TestTrue-operand test)))]
                                [(TestOne? test)
                                 (format "if (~a === 1)"
                                         (assemble-oparg (TestOne-operand test)))]
                                [(TestZero? test)
                                 (format "if (~a === 0)"
                                         (assemble-oparg (TestZero-operand test)))]
                                
                                [(TestPrimitiveProcedure? test)
                                 (format "if (typeof(~a) === 'function')"
                                         (assemble-oparg (TestPrimitiveProcedure-operand test)))]
                                
                                [(TestClosureArityMismatch? test)
                                 (format "if (! RUNTIME.isArityMatching((~a).racketArity, ~a))"
                                         (assemble-oparg (TestClosureArityMismatch-closure test))
                                         (assemble-oparg (TestClosureArityMismatch-n test)))]))
               `(, test-code
                 "{"
                 ,@(cond
                     [(set-contains? entry-points (TestAndJumpStatement-label stmt))
                      (list (assemble-jump (make-Label (TestAndJumpStatement-label stmt))))]
                     [else
                      (assemble-block-statements (BasicBlock-name
                                                  (hash-ref blockht (TestAndJumpStatement-label stmt)))
                                                 (BasicBlock-stmts 
                                                  (hash-ref blockht (TestAndJumpStatement-label stmt)))
                                                 blockht
                                                 entry-points)])
                 "} else {"
                 ,@(assemble-block-statements name (rest stmts) blockht entry-points)
                 "}")]
           
           [(GotoStatement? stmt)
            (define target (GotoStatement-target stmt))
            (cond
              [(Label? target)
               (cond
                   [(set-contains? entry-points (Label-name target))
                    (default stmt)]
                   [else
                    (log-debug (format "Assembling inlined jump into ~a" (Label-name target)) )
                    (assemble-block-statements (BasicBlock-name
                                                (hash-ref blockht (Label-name target)))
                                               (BasicBlock-stmts
                                                (hash-ref blockht (Label-name target)))
                                               blockht
                                               entry-points)])]
              [(Reg? target)
               (default stmt)]
              [(ModuleEntry? target)
               (default stmt)]
              [(CompiledProcedureEntry? target)
               (default stmt)])]
           
           
           [(PushControlFrame/Generic? stmt)
            (default stmt)]
           
           [(PushControlFrame/Call? stmt)
            (default stmt)]
           
           [(PushControlFrame/Prompt? stmt)
            (default stmt)]
           
           [(PopControlFrame? stmt)
            (default stmt)]
           
           [(PushEnvironment? stmt)
            (default stmt)]
           
           [(PopEnvironment? stmt)
            (default stmt)]
           
           [(PushImmediateOntoEnvironment? stmt)
            (default stmt)]
           [(Comment? stmt)
            (default stmt)])]))




(: basic-block-out-edges (BasicBlock -> (Listof Symbol)))
;; Returns the neighboring blocks of this block.
(define (basic-block-out-edges a-block)
  
  (: loop ((Listof UnlabeledStatement) -> (Listof Symbol)))
  (define (loop stmts)
    
    (: default (-> (Listof Symbol)))
    (define (default)
      (loop (rest stmts)))
    
    (cond [(empty? stmts)
           empty]
          [else
           (define stmt (first stmts))
           (cond
             [(DebugPrint? stmt)
              (default)]
             
             [(AssignImmediateStatement? stmt)
              (default)]
             
             [(AssignPrimOpStatement? stmt)
              (default)]
             
             [(PerformStatement? stmt)
              (default)]
             
             [(TestAndJumpStatement? stmt)
              (cons (TestAndJumpStatement-label stmt)
                    (loop (rest stmts)))]
             
             [(GotoStatement? stmt)
              (define target (GotoStatement-target stmt))
              (cond
                [(Label? target)
                 (cons (Label-name target)
                       (loop (rest stmts)))]
                [(Reg? target)
                 (default)]
                [(ModuleEntry? target)
                 (default)]
                [(CompiledProcedureEntry? target)
                 (default)])]
             
             [(PushControlFrame/Generic? stmt)
              (default)]
             
             [(PushControlFrame/Call? stmt)
              (default)]
             
             [(PushControlFrame/Prompt? stmt)
              (default)]
             
             [(PopControlFrame? stmt)
              (default)]
             
             [(PushEnvironment? stmt)
              (default)]
             
             [(PopEnvironment? stmt)
              (default)]
             
             [(PushImmediateOntoEnvironment? stmt)
              (default)]
             [(Comment? stmt)
              (default)])]))
  
  (loop (BasicBlock-stmts a-block)))








(: assemble-statement (UnlabeledStatement -> String))
;; Generates the code to assemble a statement.
(define (assemble-statement stmt)
  (string-append 
   (if (current-emit-debug-trace?)
       (format "if (typeof(window.console) !== 'undefined' && typeof(console.log) === 'function') { console.log(~s);\n}"
               (format "~a" stmt))
       "")
   (cond
     [(DebugPrint? stmt)
      (format "MACHINE.params.currentOutputPort.writeDomNode(MACHINE, $('<span/>').text(~a));" (assemble-oparg (DebugPrint-value stmt)))]
     [(AssignImmediateStatement? stmt)
      (let: ([t : (String -> String) (assemble-target (AssignImmediateStatement-target stmt))]
             [v : OpArg (AssignImmediateStatement-value stmt)])
        (t (assemble-oparg v)))]
     
     [(AssignPrimOpStatement? stmt)
      ((assemble-target (AssignPrimOpStatement-target stmt))
       (assemble-op-expression (AssignPrimOpStatement-op stmt)))]
     
     [(PerformStatement? stmt)
      (assemble-op-statement (PerformStatement-op stmt))]
     
     [(TestAndJumpStatement? stmt)
      (let*: ([test : PrimitiveTest (TestAndJumpStatement-op stmt)]
              [jump : String (assemble-jump 
                              (make-Label (TestAndJumpStatement-label stmt)))])
        ;; to help localize type checks, we add a type annotation here.
        (ann (cond
               [(TestFalse? test)
                (format "if (~a === false) { ~a }"
                        (assemble-oparg (TestFalse-operand test))
                        jump)]
               [(TestTrue? test)
                (format "if (~a !== false) { ~a }"
                        (assemble-oparg (TestTrue-operand test))
                        jump)]
               [(TestOne? test)
                (format "if (~a === 1) { ~a }"
                        (assemble-oparg (TestOne-operand test))
                        jump)]
               [(TestZero? test)
                (format "if (~a === 0) { ~a }"
                        (assemble-oparg (TestZero-operand test))
                        jump)]
               [(TestPrimitiveProcedure? test)
                (format "if (typeof(~a) === 'function') { ~a }"
                        (assemble-oparg (TestPrimitiveProcedure-operand test))
                        jump)]
               [(TestClosureArityMismatch? test)
                (format "if (! RUNTIME.isArityMatching((~a).racketArity, ~a)) { ~a }"
                        (assemble-oparg (TestClosureArityMismatch-closure test))
                        (assemble-oparg (TestClosureArityMismatch-n test))
                        jump)])
             String))]
     
     [(GotoStatement? stmt)
      (assemble-jump (GotoStatement-target stmt))]
     
     [(PushControlFrame/Generic? stmt)
      "MACHINE.control.push(new RUNTIME.Frame());"]
     
     [(PushControlFrame/Call? stmt)
      (format "MACHINE.control.push(new RUNTIME.CallFrame(~a, MACHINE.proc));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Call-label stmt)])
                (cond
                  [(symbol? label) 
                   (assemble-label (make-Label label))]
                  [(LinkedLabel? label) 
                   (assemble-label (make-Label (LinkedLabel-label label)))])))]
     
     [(PushControlFrame/Prompt? stmt)
      ;; fixme: use a different frame structure
      (format "MACHINE.control.push(new RUNTIME.PromptFrame(~a, ~a));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Prompt-label stmt)])
                (cond
                  [(symbol? label) 
                   (assemble-label (make-Label label))]
                  [(LinkedLabel? label) 
                   (assemble-label (make-Label (LinkedLabel-label label)))]))
              
              (let: ([tag : (U DefaultContinuationPromptTag OpArg)
                          (PushControlFrame/Prompt-tag stmt)])
                (cond
                  [(DefaultContinuationPromptTag? tag)
                   (assemble-default-continuation-prompt-tag)]
                  [(OpArg? tag)
                   (assemble-oparg tag)])))]
     
     [(PopControlFrame? stmt)
      "MACHINE.control.pop();"]
     
     [(PushEnvironment? stmt)
      (if (= (PushEnvironment-n stmt) 0)
          ""
          (format "MACHINE.env.push(~a);" (string-join
                                           (build-list (PushEnvironment-n stmt) 
                                                       (lambda: ([i : Natural])
                                                         (if (PushEnvironment-unbox? stmt)
                                                             "[undefined]"
                                                             "undefined")))
                                           ", ")))]
     [(PopEnvironment? stmt)
      (let: ([skip : OpArg (PopEnvironment-skip stmt)])
        (cond
          [(and (Const? skip) (= (ensure-natural (Const-const skip)) 0))
           (format "MACHINE.env.length = MACHINE.env.length - ~a;"
                   (assemble-oparg (PopEnvironment-n stmt)))]
          [else
           (format "MACHINE.env.splice(MACHINE.env.length - (~a + ~a), ~a);"
                   (assemble-oparg (PopEnvironment-skip stmt))
                   (assemble-oparg (PopEnvironment-n stmt))
                   (assemble-oparg (PopEnvironment-n stmt)))]))]
     
     [(PushImmediateOntoEnvironment? stmt)
      (format "MACHINE.env.push(~a);"
              (let: ([val-string : String
                                 (cond [(PushImmediateOntoEnvironment-box? stmt)
                                        (format "[~a]" (assemble-oparg (PushImmediateOntoEnvironment-value stmt)))]
                                       [else
                                        (assemble-oparg (PushImmediateOntoEnvironment-value stmt))])])
                val-string))]
     [(Comment? stmt)
      ;; TODO: maybe comments should be emitted as JavaScript comments.
      ""])))


(define-predicate natural? Natural)

(: ensure-natural (Any -> Natural))
(define (ensure-natural x)
  (if (natural? x)
      x
      (error 'ensure-natural)))


