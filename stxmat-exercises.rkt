#lang at-exp planet dyoo/whalesong
(require (planet dyoo/whalesong/js))

;;; STATUS:
;    - Menu med alle øvelserne
;    - Header og footer ?
;    - Forklaring: Link til blog post?
;    - Credits

;;; Whalesong Wishlist
; - get value of JS variable
; - call JS function directly
; - primitives: js-object? number? min max build-list

;;; Missing Whalesong primitives (for the time being)

; access to JavaScript object fields. E.g. event.keyCode.

(define (min x y)
  (if (< x y) x y))

(define (max x y)
  (if (> x y) x y))

(define (interval from to)
  ; from inclusive, to exclusive
  (if (>= from to)
      '()
      (cons from (interval (+ from 1) to))))

(define (string-append-n str n)
  (if (<= n 0)
      ""
      (let ([r (string-append-n str (- n 1))])
        (string-append str r))))

;;;
;;; Addition Exercises
;;; 

; The exercises are of the form x+y where x and y are small integers.
; The stuedent must answer correctly, before a new problem is generated.

; The code for the problem and for the exercise framework are separated.

(define-struct exercise (title summary new-problem! problem-description hints check-answer))

(define (addition-exercise)
  (define x 0)
  (define y 0)
  
  (define (new-problem!)
    (set! x (random 10))
    (set! y (random 10)))

  (define (problem-description)
    @stringify{<div>Calculate the sum.</div>
               <div> $$@x + @y = \ ?$$ </div>})

  (define (hints)
    (list @stringify{The sum of @x and @y is the same as the sum of @(sub1 x) and @(add1 y).}
          @stringify{The sum @x + @y is equal to @(+ x y).}))
  
  (define (check-answer ans)
    (= (+ x y) ans))
  
  (new-problem!)
  (make-exercise "Addition" 
                 "Sums of one digit numbers."
                 new-problem! problem-description hints check-answer))

(define (power1-exercise)
  (define a 0)
  (define n 0)
  
  (define (new-problem!)
    (set! a (random 5))
    (set! n (random 5)) ; [0,4]
    (when (= a n 0)
      (new-problem!)))

  (define (problem-description)
    @stringify{<div>Calculate the power.</div>
               <div> $$@|a|^@n = \ ?$$ </div>})

  (define (hints)
    (if (zero? n)
        (list @stringify{The power $a^0$ is defined to be $1$.}
              @stringify{$$@|a|^0 = 1.$$})    
        (list @stringify{The power $a^n$ is $a$ multiplied with itself $n$ times.}
              @stringify{$$a^@|n| = a @(string-append-n "\\cdot a" (sub1 n)).$$}
              @stringify{$$@|a|^@|n| = @a @(string-append-n @stringify{\cdot @|a|} (sub1 n)).$$}
              @stringify{$$@|a|^@|n| = @(expt a n).$$})))
  
  (define (check-answer ans)
    (= (expt a n) ans))
  
  (new-problem!)
  (make-exercise "Powers with natural exponents" 
                 "Powers with natural exponents."
                 new-problem! problem-description hints check-answer))

(define (power2-exercise)
  (define a 0)
  (define n 0)
  
  (define (new-problem!)
    (set! a (+ 1 (random 3)))    ; [1,3]
    (set! n (- -1 (random 4))))  ;[-5,-1]
    
  (define (problem-description)
    @stringify{<div>Calculate the power.</div>
               <div> $$@|a|^{@n} = \ ?$$ </div>})

  (define (hints)
    (list @stringify{Use the formula: $$a^{-n} = \frac{1}{a^n}$$}
          @stringify{$$a^{@|n|} = \frac{1}{a^@(- n)}.$$}
          @stringify{$$@|a|^{@n} = \frac{1}{@|a|^{@(- n)}}.$$}
          @stringify{$$@|a|^{@n} = \frac{1}{@|a|^{@(- n)}} = \frac{1}{@|a|@(string-append-n @stringify{\cdot @|a|} (sub1 (- n)))}. $$}
          @stringify{$$@|a|^{@n} = \frac{1}{@(expt a (- n))}.$$ Enter the result as $1/@(expt a (- n))$.}))
  
  (define (check-answer ans)
    (= (expt a n) ans))
  
  (new-problem!)
  (make-exercise "Powers with negative exponents" 
                 "Powers with natural exponents."
                 new-problem! problem-description hints check-answer))

(define (combine-exercises title summary . exercises)
  ; combine a number of exercises into 1
  ; all exercises are used with the same probability
  (let ([all (map (lambda (exercise) (exercise)) exercises)])
    (let ([current 0])
      (define (new-problem!)
        (set! current (random (length all)))
        ((exercise-new-problem! (list-ref all current))))
      (define (problem-description)
        ((exercise-problem-description (list-ref all current))))
      (define (hints)
        ((exercise-hints (list-ref all current))))
      (define (check-answer ans)
        ((exercise-check-answer (list-ref all current)) ans))
      (make-exercise title summary new-problem! problem-description hints check-answer))))

(define power-all-kinds-exercise 
  (combine-exercises "Powers" "Combines all power exercises" 
                     power1-exercise power2-exercise))

(define current-exercise (power-all-kinds-exercise))

;;;
;;; Problem related
;;;


;;;
;;; General Utilities
;;;

(define (stringify . xs)
  ; TODO: FIX when number? becomes available 
  (apply string-append
         (map (lambda (x) (if (string? x) x (number->string x)))
              xs)))

;;; 
;;; Exercise Framework 
;;;

;;; Session State
(define streak-current 0)  ; correct answers in a row
(define streak-max 0)      ; longest streak so far

;;; General DOM functions

(define (append-to-body . strs)
  (for-each (lambda (str) (call-method ($ str) "appendTo" body)) strs))

;;; General jQuery utilities

; The methods of the proxy object directly calls JS functions.
; See the initial <script> from in generate-body.
(define js-proxy 'uninitialized)
(define (store-js-proxy js-obj)
  (set! js-proxy js-obj))
(define (call-proxy name . args)
  (unless (symbol? js-proxy)
    (apply call-method js-proxy name args)))

(define (run-mathjax)
  (js-eval "MathJax.Hub.Queue(['Typeset',MathJax.Hub]);"))

(define (create-scratchpad)
  (js-eval "Scratchpad('#scratchpad');"))

(define (js-eval expr)
  (call-proxy "jsEval" expr))

;;; VIEW UPDATERS

;;; Problem area

(define (update-exercise-title)
  (call-method ($ "#exercise_title") "replaceWith"
               ($ @stringify{ <div id='exercise_title'> @(exercise-title current-exercise) </div>})))

(define (update-problem-area str)
  (call-method ($ "#problem_area") "replaceWith" 
               ($ @stringify{ <div id='problem_area'> @str </div>}))
  (run-mathjax))

;;; Hint area
(define (hide-hint-area)
  (call-method ($ "#hint_area") "css" "display" "none"))

(define (show-hint-area)
  (call-method ($ "#hint_area") "css" "display" "block"))

(define (clear-hint-area)
  (call-method ($ "#hint_area") "replaceWith" 
               ($ @stringify{ <div id='hint_area'> </div>})))

(define (append-to-hint-area str)
  (call-method ($ "#hint_area") "append" 
               ($ @stringify{ <div> @str </div>})))

(define (change-hint-button-text str)
  (call-method ($ "#hint_button") "val" str))

;;; Scratchpad
(define (show-scratchpad)
  (call-method ($ "#scratchpad") "css" "display" "inline")
  (call-method ($ "#scratchpad_toggle") "html" "Hide ScratchPad"))

(define (hide-scratchpad)
  (call-method ($ "#scratchpad") "css" "display" "none")
  (call-method ($ "#scratchpad_toggle") "html" "Show ScratchPad"))


;;; Answer area
(define (clear-answer-input)
  (call-method ($ "#answer_input") "replaceWith" 
               @stringify{<input id='answer_input' type='text' value='' 
                                 onkeyup='@(call-plt-function1 'on-keyup/answer-input "event.keyCode")' />}))

(define (set-focus-to-answer-input)
  (call-method ($ "#answer_input") "focus"))

(define (change-answer-button-text str)
  (call-method ($ "#answer_button") "val" str))

(define (face state)
  (call-method ($ "#happy") "css" "display" 
               (if (eq? state 'happy) "inline" "none"))
  (call-method ($ "#sad") "css" "display" 
               (if (eq? state 'sad) "inline" "none")))

;;; Streak bar
(define (update-streak-bar)
  (let* ([total-bar-width (- 300 3)]  ; 3 borders a 1px = 3px
         [current-width   (quotient (* total-bar-width streak-current) 10)])
    (call-method ($ "#streak_bar_left")  "css" "width" (max (min current-width total-bar-width) 0))
    (call-method ($ "#streak_bar_right") "css" "width" (max (min (- total-bar-width current-width) total-bar-width) 0))
    (call-method ($ "#streak_bar_current") "html" @stringify{<span>@(number->string streak-current)</span>})))

;;; VIEW GETTERS

(define (get-user-answer)
  (let ([v (call-method ($ "#answer_input") "val")])
    (cond [(string? v) (string->number v)]
          [else #f])))

;;; CONTROL

(define on-answer-button
  (let ([state 'next])
    (lambda ()
      (case state
        [(check)   (let ([ans (get-user-answer)])
                     (if (and ans ((exercise-check-answer current-exercise) ans))
                         (begin
                           (set! state 'next)
                           (set! streak-current (+ streak-current 1))
                           (change-answer-button-text "Correct! Next Question...")
                           (face 'happy))
                         (begin
                           (set! state 'check)
                           (set! streak-current 0)
                           (face 'sad))))]
        [(next)    (begin
                     (set! state 'check)
                     (change-answer-button-text "Check Answer")
                     (face 'none)
                     ((exercise-new-problem! current-exercise))
                     (update-problem-area 
                      ((exercise-problem-description current-exercise)))
                     (reset-hint-index)
                     (change-hint-button-text "Get Hint")
                     (hide-hint-area)
                     (clear-answer-input)
                     (set-focus-to-answer-input))]
        [else     
         (alert "Error: Unknown state in 'on-answer-button'")])
      
      (update-streak-bar))))

(define (on-keyup/answer-input event)
  (if (= event 13) ; enter
      (on-answer-button)
      (void)))

(define hint-index -1)

(define (reset-hint-index)
  (set! hint-index -1))

(define (on-hint-button)
  (let ([hints ((exercise-hints current-exercise))])
    (set! streak-current 0)
    (update-streak-bar)
    (change-hint-button-text "Get another hint")
    (set! hint-index (+ hint-index 1))
    (when (zero? hint-index)
      (clear-hint-area)
      (show-hint-area))
    (if (>= hint-index (length hints))
        (if (= hint-index (length hints))
            (append-to-hint-area "There are no more hints.")
            (void))
        (begin
          (append-to-hint-area (list-ref hints hint-index))
          (run-mathjax)))))

(define on-toggle-scratchpad
  (let ([state 'hidden]
        [initialized #f])
    (lambda ()
      (unless initialized
        (create-scratchpad)
        (set! initialized #t))
      (case state
        [(hidden) 
         (set! state 'shown)
         (show-scratchpad)]
        [(shown)  
         (set! state 'hidden)
         (hide-scratchpad)]))))

;;; VIEW

(define js-identity "function(v){return v;}")
(define (plt-function name)
  (format "plt.baselib.functions.asJavaScriptFunction( plt.runtime.lookupInMains(\"~a\") )"
          name))
(define (call-plt-thunk name)
  (format "storeProxy();(~a)(~a,~a)" (plt-function (symbol->string name)) js-identity js-identity))
(define (call-plt-function1 name arg1)
  (format "storeProxy();(~a)(~a,~a,~a)" (plt-function (symbol->string name)) js-identity js-identity arg1))


(define (generate-body)
  (define id (lambda (x) x))  
  (append-to-body 
   (let* ([streak-bar-height 30]
          [sbh streak-bar-height])
   @stringify{
     <script>
         function Proxy() {}
         Proxy.prototype.jsEval = function(expr) { return eval(expr); };
         var proxy = new Proxy();
         var storeProxy = function(){@(format "(~a)(~a,~a,proxy)" (plt-function "store-js-proxy") js-identity js-identity);};
         </script>
     <style>
         #scratchpad { padding-left: 30px; overflow: hidden; display: inline; }
         #scratchpad svg { position: absolute; z-index: 1; min-height: 350px; width: 50%; height: 85%;}

         #exercise_title{font-size: xx-large; display: inline; height:@(+ streak-bar-height 5)}
         #streak_bar_area{float:right; position: relative;}
         #streak_bar{float:left; position:relative; border:1px; border-color: black; border-style: solid;}
         #streak_bar_left {float:left; height: @(id sbh)px; position:relative; width:50px; background-color: green; 
                                               border-right: solid; border-width: 1px;}
         #streak_bar_right{float:left; height: @(id sbh)px; position:relative; width:50px; background-color: red;}
         #streak_bar_current{position:absolute; left: 5px; bottom: 5px; color: white; display: inline; z-index: 1;}
         #title_spacer{height: 20px; clear:both;}
         #problem_and_answer_area{}
         #problem_and_hint_area{float: left;}
         #problem_area{padding-left: 50px;}
         #hint_area{padding-left: 50px; margin-top: 10px; border: 1px;}
         #answer_and_help_area{float: right; width: 300px;}
         #scratchpad_toggle{ text-decoration: underline; color: dodgerblue;}
         #answer_area{border: 1px; border-style: solid; border-color: black; background-color: lightblue; padding: 10px;}
         #answer_area_title{position:relative; font-size:large; margin-bottom: 10px; }
         #answer_input{margin-bottom: 10px}
         #help_area{border: 1px; border-style: solid; border-color: black; background-color: lightblue; padding: 10px; margin-top: 15px;}
         #help_area_title{position:relative; font-size:large; margin-bottom: 10px;}
     </style>
     <div id='title_area'>
         <div id='exercise_title'>Addition</div>
         <div id='streak_bar_area'> 
             <div id='streak_bar'>
                 <div id='streak_bar_current'>0</div>
                 <div id='streak_bar_left'>  </div>   
                 <div id='streak_bar_right'> </div>   
             </div>
             <div>Streak...</div>
         </div>
     </div>
     <div id='title_spacer'></div>
     <div id='problem_and_answer_area'>
         <div id='problem_and_hint_area'>
             <div id='scratchpad'><div></div></div>
             <div id='problem_area'>1+2=?</div>
             <div id='hint_area'>Hints are displayed here</div>
         </div>
         <div id='answer_and_help_area'>
             <a id='scratchpad_toggle' onclick='@(call-plt-thunk 'on-toggle-scratchpad)'>Show ScratchPad</a>
             <div id='answer_area'>
                 <div id='answer_area_title'>Answer</div>
                 <div><input id='answer_input' type='text' value='7' /></div>
                 <input id='answer_button' type='button' onclick='@(call-plt-thunk 'on-answer-button)' value='Check Answer'/>
                 <div><img id='sad' style='display: none;' src='face-sad.gif'/></div>
                 <div><img id='happy' style='display: none;' src='face-happy.gif'/></div>
             </div>
             <div id='help_area'>
                 <div id='help_area_title'>Need help?</div>
                 <input id='hint_button' type='button' onclick='storeProxy();@(call-plt-thunk 'on-hint-button)' value='Get Hint'/>
                 <p></p>
                 <div>Beware: Getting a hint will reset your streak</div>
             </div>
         </div>
     </div>})))

;;; START

(void ((exercise-new-problem! current-exercise))
      (generate-body)
      (update-exercise-title)
      (on-answer-button)
      (clear-answer-input)
      (update-streak-bar)
      (js-eval "storeProxy();")
      (run-mathjax))
