#lang typed/racket/base


;; Assembles the statement stream into JavaScript.


(require "assemble-structs.rkt"
         "assemble-helpers.rkt"
         "assemble-expression.rkt"
         "assemble-perform-statement.rkt"
         "fracture.rkt"
         "../compiler/il-structs.rkt"
         "../sets.rkt"
         "../helpers.rkt"
         racket/string
         racket/list
         racket/match)
(require/typed "../logger.rkt"
               [log-debug (String -> Void)])

(provide assemble/write-invoke
         assemble-statement)





;; Parameter that controls the generation of a trace.
(define current-emit-debug-trace? (make-parameter #f))





(: assemble/write-invoke ((Listof Statement) Output-Port -> Void))
;; Writes out the JavaScript code that represents the anonymous invocation expression.
;; What's emitted is a function expression that, when invoked, runs the
;; statements.
(define (assemble/write-invoke stmts op)
  (display "(function(M, success, fail, params) {\n" op)
  (display "var param;\n" op)
  (display "var RT = plt.runtime;\n" op)
  
  (define-values (basic-blocks entry-points) (fracture stmts))

  (define function-entry-and-exit-names
    (list->set (get-function-entry-and-exit-names stmts)))

  (: blockht : Blockht)
  (define blockht (make-hash))
  
  (for ([b basic-blocks])
    (hash-set! blockht (BasicBlock-name b) b))

  (write-blocks basic-blocks
                blockht
                (list->set entry-points)
                function-entry-and-exit-names
                op)
  
  (write-linked-label-attributes stmts blockht op)
  
  (display "M.params.currentErrorHandler = fail;\n" op)
  (display "M.params.currentSuccessHandler = success;\n" op)
  (display  #<<EOF
for (param in params) {
    if (params.hasOwnProperty(param)) {
        M.params[param] = params[param];
    }
}
EOF
           op)
  (fprintf op "M.trampoline(~a, true); })"
           (assemble-label (make-Label (BasicBlock-name (first basic-blocks)))
                           blockht)))



(: write-blocks ((Listof BasicBlock) Blockht (Setof Symbol) (Setof Symbol) Output-Port -> Void))
;; Write out all the basic blocks associated to an entry point.
(define (write-blocks blocks blockht entry-points function-entry-and-exit-names op)  
  
  ;; Since there may be cycles between the blocks, we cut the cycles by
  ;; making them entry points as well.
  (insert-cycles-as-entry-points! entry-points blockht)

  (set-for-each (lambda: ([s : Symbol])
                  (log-debug (format "Emitting code for basic block ~s" s))
                  (assemble-basic-block (hash-ref blockht s) 
                                        blockht
                                        entry-points
                                        function-entry-and-exit-names
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
          #;(unless (set-contains? entry-points next-to-visit)
            (log-debug (format "Promoting ~a to an entry point" next-to-visit))
            (set-insert! entry-points next-to-visit))
          (loop (rest queue))]
         [else
          (set-insert! visited next-to-visit)
          (set-insert! entry-points next-to-visit)
          (loop (list-union (basic-block-out-edges (hash-ref blockht next-to-visit))
                            (rest queue)))])]))

  (loop (set->list entry-points)))





(: write-linked-label-attributes ((Listof Statement) Blockht Output-Port -> 'ok))
(define (write-linked-label-attributes stmts blockht op)
  (cond
    [(empty? stmts)
     'ok]
    [else
     (let: ([stmt : Statement (first stmts)])
       
       (define (next) (write-linked-label-attributes (rest stmts) blockht op))
       
       (cond
         [(symbol? stmt)
          (next)]
         [(LinkedLabel? stmt)
          ;; Setting up multiple-value-return.
          ;; Optimization: in the most common case (expecting only one), we optimize away
          ;; the assignment, because there's a distinguished instruction, and it's implied
          ;; that if .mvr is missing, that the block only expects one.
          (define linked-to-block (hash-ref blockht (LinkedLabel-linked-to stmt)))
          (cond
           [(block-looks-like-context-expected-values? linked-to-block)
            => (lambda (expected)
                 (cond
                  [(= expected 1)
                   (void)]
                  [else
                   (fprintf op "~a.mvr=RT.si_context_expected(~a);\n"
                            (munge-label-name (make-Label (LinkedLabel-label stmt)))
                            expected)]))]
           [else
            (fprintf op "~a.mvr=~a;\n" 
                     (munge-label-name (make-Label (LinkedLabel-label stmt)))
                     (assemble-label (make-Label (LinkedLabel-linked-to stmt)) blockht))])
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






(: assemble-basic-block (BasicBlock Blockht (Setof Symbol) (Setof Symbol) Output-Port -> 'ok))
(define (assemble-basic-block a-basic-block blockht entry-points function-entry-and-exit-names op)
  (cond
   [(block-looks-like-context-expected-values? a-basic-block)
    =>
    (lambda (expected)
      (cond
       [(= expected 1)
        'ok]
       [else
        (fprintf op "~a=RT.si_context_expected(~a);\n"
                 (munge-label-name (make-Label (BasicBlock-name a-basic-block)))
                 expected)
        'ok]))]

   [(block-looks-like-pop-multiple-values-and-continue? a-basic-block)
    =>
    (lambda (target)
      (fprintf op "~a=RT.si_pop_multiple-values-and-continue(~a);"
               (munge-label-name (make-Label (BasicBlock-name a-basic-block)))
               target))]
   [else
    (default-assemble-basic-block a-basic-block blockht entry-points function-entry-and-exit-names op)]))



(: default-assemble-basic-block (BasicBlock Blockht (Setof Symbol) (Setof Symbol) Output-Port -> 'ok))
(define (default-assemble-basic-block a-basic-block blockht entry-points function-entry-and-exit-names op)
  (cond
   [(set-contains? function-entry-and-exit-names (BasicBlock-name a-basic-block))
    (fprintf op "var ~a=function(M){if(--M.cbt<0){throw ~a;}\n"
             (assemble-label (make-Label (BasicBlock-name a-basic-block)) blockht)
             (assemble-label (make-Label (BasicBlock-name a-basic-block)) blockht))]
   [else
    (fprintf op "var ~a=function(M){\n"
             (assemble-label (make-Label (BasicBlock-name a-basic-block)) blockht))])
  (assemble-block-statements (BasicBlock-name a-basic-block)
                             (BasicBlock-stmts a-basic-block)
                             blockht
                             entry-points
                             op)
  (display "};\n" op)
  'ok)




(: assemble-block-statements (Symbol (Listof UnlabeledStatement) Blockht (Setof Symbol) Output-Port -> 'ok))
(define (assemble-block-statements name stmts blockht entry-points op)
  
  (: default (UnlabeledStatement -> 'ok))
  (define (default stmt)
    (when (and (empty? (rest stmts))
               (not (GotoStatement? stmt)))
      (log-debug (format "Last statement of the block ~a is not a goto" name)))

    (display (assemble-statement stmt blockht) op)
    (newline op)
    (assemble-block-statements name
                               (rest stmts) 
                               blockht
                               entry-points
                               op))
  
  (cond [(empty? stmts)
         'ok]
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
                                 (format "if(~a===false)"
                                         (assemble-oparg (TestFalse-operand test)
                                                         blockht))]
                                [(TestTrue? test)
                                 (format "if(~a!==false)"
                                         (assemble-oparg (TestTrue-operand test)
                                                         blockht))]
                                [(TestOne? test)
                                 (format "if(~a===1)"
                                         (assemble-oparg (TestOne-operand test)
                                                         blockht))]
                                [(TestZero? test)
                                 (format "if(~a===0)"
                                         (assemble-oparg (TestZero-operand test)
                                                         blockht))]
                                                                
                                [(TestClosureArityMismatch? test)
                                 (format "if(!RT.isArityMatching((~a).racketArity,~a))"
                                         (assemble-oparg (TestClosureArityMismatch-closure test)
                                                         blockht)
                                         (assemble-oparg (TestClosureArityMismatch-n test)
                                                         blockht))]))
               (display test-code op)
               (display "{" op)
               (cond
                [(set-contains? entry-points (TestAndJumpStatement-label stmt))
                 (display (assemble-jump (make-Label (TestAndJumpStatement-label stmt))
                                         blockht) op)]
                [else
                 (assemble-block-statements (BasicBlock-name
                                             (hash-ref blockht (TestAndJumpStatement-label stmt)))
                                            (BasicBlock-stmts 
                                             (hash-ref blockht (TestAndJumpStatement-label stmt)))
                                            blockht
                                            entry-points
                                            op)])
               (display "}else{" op)
               (assemble-block-statements name (rest stmts) blockht entry-points op)
               (display "}" op)
               'ok]
           
           [(GotoStatement? stmt)
            (let loop ([stmt stmt])
              (define target (GotoStatement-target stmt))
              (cond
               [(Label? target)
                (define target-block (hash-ref blockht (Label-name target)))
                (define target-name (BasicBlock-name target-block))
                (define target-statements (BasicBlock-stmts target-block))
                (cond
                 ;; Optimization: if the target block consists of a single goto,
                 ;; inline and follow the goto.
                 [(and (not (empty? target-statements))
                       (= 1 (length target-statements))
                       (GotoStatement? (first target-statements)))
                  (loop (first target-statements))]
                 [(set-contains? entry-points (Label-name target))
                  (display (assemble-statement stmt blockht) op)
                  'ok]
                 [else
                  (log-debug (format "Assembling inlined jump into ~a" (Label-name target)) )
                  (assemble-block-statements target-name
                                             target-statements
                                             blockht
                                             entry-points
                                             op)])]
               [(Reg? target)
                (display (assemble-statement stmt blockht) op)
                'ok]
               [(ModuleEntry? target)
                (display (assemble-statement stmt blockht) op)
                'ok]
               [(CompiledProcedureEntry? target)
                (display (assemble-statement stmt blockht) op)
                'ok]))]

           
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








(: assemble-statement (UnlabeledStatement Blockht -> String))
;; Generates the code to assemble a statement.
(define (assemble-statement stmt blockht)
  (define assembled
    (cond
     [(DebugPrint? stmt)
      (format "M.params.currentOutputPort.writeDomNode(M, $('<span/>').text(~a));"
              (assemble-oparg (DebugPrint-value stmt)
                              blockht))]
     [(AssignImmediateStatement? stmt)
      (let: ([t : (String -> String) (assemble-target (AssignImmediateStatement-target stmt))]
             [v : OpArg (AssignImmediateStatement-value stmt)])
        (t (assemble-oparg v blockht)))]
     
     [(AssignPrimOpStatement? stmt)
      ((assemble-target (AssignPrimOpStatement-target stmt))
       (assemble-op-expression (AssignPrimOpStatement-op stmt)
                               blockht))]
     
     [(PerformStatement? stmt)
      (assemble-op-statement (PerformStatement-op stmt) blockht)]
     
     [(TestAndJumpStatement? stmt)
      (let*: ([test : PrimitiveTest (TestAndJumpStatement-op stmt)]
              [jump : String (assemble-jump 
                              (make-Label (TestAndJumpStatement-label stmt))
                              blockht)])
        ;; to help localize type checks, we add a type annotation here.
        (ann (cond
               [(TestFalse? test)
                (format "if(~a===false){~a}"
                        (assemble-oparg (TestFalse-operand test)
                                        blockht)
                        jump)]
               [(TestTrue? test)
                (format "if(~a!==false){~a}"
                        (assemble-oparg (TestTrue-operand test)
                                        blockht)
                        jump)]
               [(TestOne? test)
                (format "if(~a===1){~a}"
                        (assemble-oparg (TestOne-operand test)
                                        blockht)
                        jump)]
               [(TestZero? test)
                (format "if(~a===0){~a}"
                        (assemble-oparg (TestZero-operand test)
                                        blockht)
                        jump)]
               [(TestClosureArityMismatch? test)
                (format "if(!RT.isArityMatching((~a).racketArity,~a)){~a}"
                        (assemble-oparg (TestClosureArityMismatch-closure test)
                                        blockht)
                        (assemble-oparg (TestClosureArityMismatch-n test)
                                        blockht)
                        jump)])
             String))]
     
     [(GotoStatement? stmt)
      (assemble-jump (GotoStatement-target stmt)
                     blockht)]
     
     [(PushControlFrame/Generic? stmt)
      "M.c.push(new RT.Frame());"]
     
     [(PushControlFrame/Call? stmt)
      (format "M.c.push(new RT.CallFrame(~a,M.p));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Call-label stmt)])
                (cond
                  [(symbol? label) 
                   (assemble-label (make-Label label)
                                   blockht)]
                  [(LinkedLabel? label) 
                   (assemble-label (make-Label (LinkedLabel-label label))
                                   blockht)])))]
     
     [(PushControlFrame/Prompt? stmt)
      ;; fixme: use a different frame structure
      (format "M.c.push(new RT.PromptFrame(~a,~a));" 
              (let: ([label : (U Symbol LinkedLabel) (PushControlFrame/Prompt-label stmt)])
                (cond
                  [(symbol? label) 
                   (assemble-label (make-Label label)
                                   blockht)]
                  [(LinkedLabel? label) 
                   (assemble-label (make-Label (LinkedLabel-label label))
                                   blockht)]))
              
              (let: ([tag : (U DefaultContinuationPromptTag OpArg)
                          (PushControlFrame/Prompt-tag stmt)])
                (cond
                  [(DefaultContinuationPromptTag? tag)
                   (assemble-default-continuation-prompt-tag)]
                  [(OpArg? tag)
                   (assemble-oparg tag blockht)])))]
     
     [(PopControlFrame? stmt)
      "M.c.pop();"]
     
     [(PushEnvironment? stmt)
      (cond [(= (PushEnvironment-n stmt) 0)
             ""]
            [(PushEnvironment-unbox? stmt)
             (format "M.e.push(~a);" (string-join
                                      (build-list (PushEnvironment-n stmt) 
                                                  (lambda: ([i : Natural])
                                                               "[undefined]"))
                                      ", "))]
            [else
             (format "M.e.length+=~a;" (PushEnvironment-n stmt))])]
     [(PopEnvironment? stmt)
      (let: ([skip : OpArg (PopEnvironment-skip stmt)])
        (cond
          [(and (Const? skip) (= (ensure-natural (Const-const skip)) 0))
           (cond [(equal? (PopEnvironment-n stmt)
                          (make-Const 1))
                  "M.e.pop();"]
                 [else
                  (format "M.e.length-=~a;"
                          (assemble-oparg (PopEnvironment-n stmt) blockht))])]
          [else
           (format "M.e.splice(M.e.length-(~a+~a),~a);"
                   (assemble-oparg (PopEnvironment-skip stmt) blockht)
                   (assemble-oparg (PopEnvironment-n stmt) blockht)
                   (assemble-oparg (PopEnvironment-n stmt) blockht))]))]
     
     [(PushImmediateOntoEnvironment? stmt)
      (format "M.e.push(~a);"
              (let: ([val-string : String
                                 (cond [(PushImmediateOntoEnvironment-box? stmt)
                                        (format "[~a]" (assemble-oparg (PushImmediateOntoEnvironment-value stmt)
                                                                       blockht))]
                                       [else
                                        (assemble-oparg (PushImmediateOntoEnvironment-value stmt)
                                                        blockht)])])
                val-string))]
     [(Comment? stmt)
      ;; TODO: maybe comments should be emitted as JavaScript comments.
      ""]))
  (cond
   #;[(current-emit-debug-trace?)
    (string-append 
     (format "if(typeof(window.console)!=='undefined'&&typeof(window.console.log)==='function'){window.console.log(~s);\n}"
               (format "~a" stmt))
     assembled)]
   [else
    assembled]))


(define-predicate natural? Natural)

(: ensure-natural (Any -> Natural))
(define (ensure-natural n)
  (if (natural? n)
      n
      (error 'ensure-natural)))



(: get-function-entry-and-exit-names ((Listof Statement) -> (Listof Symbol)))
(define (get-function-entry-and-exit-names stmts)
  (cond
   [(empty? stmts)
    '()]
   [else
    (define first-stmt (first stmts))
    (cond
     [(LinkedLabel? first-stmt)
      (cons (LinkedLabel-label first-stmt)
            (cons (LinkedLabel-linked-to first-stmt)
                  (get-function-entry-and-exit-names (rest stmts))))]
     [(AssignPrimOpStatement? first-stmt)
      (define op (AssignPrimOpStatement-op first-stmt))
      (cond
       [(MakeCompiledProcedure? op)
        (cons (MakeCompiledProcedure-label op)
              (get-function-entry-and-exit-names (rest stmts)))]
       [(MakeCompiledProcedureShell? first-stmt)
        (cons (MakeCompiledProcedureShell-label op)
              (get-function-entry-and-exit-names (rest stmts)))]
       [else
        (get-function-entry-and-exit-names (rest stmts))])]
     [else
      (get-function-entry-and-exit-names (rest stmts))])]))
