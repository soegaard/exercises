#lang at-exp planet dyoo/whalesong
(require (planet dyoo/whalesong/js))
(require "lang/primitives.rkt"
         "lang/do.rkt"
         "utilities/random.rkt"
         "utilities/string.rkt")

; In the process of introducing radio buttons.
; Buttons are now showable / hideable
; TODO: Update the answer area after new-problem! is called.

;;; STATUS:
;  Missing
;    - Menu with all exercises
;    - Header og footer ?
;    - Credits
; Bugs:
;    - MathJax doesn't run on the very first exercise
;    - 1/ 3  is not parsed as 1/3 due to the space

; The code for the problem and for the exercise framework are separated.

(define-struct exercise (title summary new-problem! problem-description hints check-answer answer-type))
(define-struct answer (html css show get))

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

(define (show-answer-field n)
  (unless (<= 1 n 3)
    (error "There are only 3 answer fields."))
  (call-method ($ @stringify{#answer_input_@|n|}) "show"))

(define (show-answer-label n)
  (unless (<= 1 n 3)
    (error "There are only 3 answer labels."))
  (call-method ($ @stringify{#answer_input_label_@|n|}) "show"))

(define (show-multiple-choice-radio-button n)
  (unless (<= 0 n 4)
    (error "There are only 5 radio buttons."))
  (call-method ($ @stringify{#mc@|n|}) "show")
  (call-method ($ @stringify{#mc@|n|_val}) "show"))

(define (get-answer-field n)
  (let ([v (call-method ($ (format "#answer_input_~a" n)) "val")])
    (cond [(not (string? v)) #f]
          [else v])))

(define (get-multiple-choice)
  ; Note: This relies on the the "name" attributes in the html
  (string->number (call-method ($ ":checked") "val")))

(define (clear-answer-input)
  (define (clear n)
    (call-method ($ @stringify{#answer_input_@|n|}) "replaceWith" 
                 @stringify{<input id='answer_input_@|n|' type='text' value='' 
                            onkeyup='@(call-plt-function1 'on-keyup/answer-input "event.keyCode")' />}))
  (for-each clear '(1 2 3)))

(define (hide-answer-labels)
  (define (hide n)
    (call-method ($ @stringify{#answer_input_label_@|n|}) "hide")) 
  (for-each hide '(1 2 3)))

(define (hide-answer-inputs)
  (define (hide n)
    (call-method ($ @stringify{#answer_input_@|n|}) "hide")) 
  (for-each hide '(1 2 3)))

(define (hide-multiple-choice-radio-buttons)
  (define (hide n)
    (call-method ($ @stringify{#mc@|n|}) "hide")
    (call-method ($ @stringify{#mc@|n|_val}) "hide")) 
  (for-each hide '(0 1 2 3 4)))

(define (hide-answer-inputs-and-labels)
  (hide-answer-labels)
  (hide-answer-inputs))


(define (set-focus-to-answer-input)
  (call-method ($ "#answer_input_1") "focus"))

(define (change-answer-button-text str)
  (call-method ($ "#answer_button") "val" str))

(define (change-multiple-choice-text n str)
  (call-method ($ @stringify{#mc@|n|_val}) "html" 
               (string-append str "<br/>")))

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


;;; CONTROL

(define on-answer-button
  (let ([state 'next])
    (lambda ()
      (case state
        [(check)   (let ([ans ((answer-get (exercise-answer-type current-exercise)))])
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
                     (clear-answer-input) ; clear before hide...
                     (hide-answer-inputs-and-labels)
                     ((answer-show (exercise-answer-type current-exercise)))
                     (reset-hint-index)
                     (change-hint-button-text "Get Hint")
                     (hide-hint-area)
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
                #answer_input_1{margin-bottom: 10px}
                #answer_input_2{margin-bottom: 10px; display: inline;}
                #answer_input_3{margin-bottom: 10px; display: inline;}
                #answer_input_label_1{display: inline; margin-right: 5px;}
                #answer_input_label_2{display: inline; margin-right: 5px;}
                #answer_input_label_3{display: inline; margin-right: 5px;}
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
                    <div><div id='answer_input_label_1'>a</div><input id='answer_input_1' type='text' value='7' /></div>
                    <div><div id='answer_input_label_2'>b</div><input id='answer_input_2' type='text' value='8' /></div>
                    <div><div id='answer_input_label_3'>c</div><input id='answer_input_3' type='text' value='9' /></div>
                    <div><input id="mc0" type="radio" name="mc" value="0" checked=""/> <span id="mc0_val">Foo0<br/></span>
                         <input id="mc1" type="radio" name="mc" value="1"/>            <span id="mc1_val">Foo1<br/></span>
                         <input id="mc2" type="radio" name="mc" value="2"/>            <span id="mc2_val">Foo2<br/></span>
                         <input id="mc3" type="radio" name="mc" value="3"/>            <span id="mc3_val">Foo3<br/></span>
                         <input id="mc4" type="radio" name="mc" value="4"/>            <span id="mc4_val">Foo4<br/></span>
                    </div>
                    <input id='answer_button' type='button' onclick='@(call-plt-thunk 'on-answer-button)' value='Check Answer'/>
                    <div><img id='sad' style='display: none;' src='../pics/face-sad.gif'/></div>
                    <div><img id='happy' style='display: none;' src='../pics/face-happy.gif'/></div>
                </div>
                <div id='help_area'>
                <div id='help_area_title'>Need help?</div>
                <input id='hint_button' type='button' onclick='storeProxy();@(call-plt-thunk 'on-hint-button)' value='Get Hint'/>
                <p></p>
                <div>Beware: Getting a hint will reset your streak</div>
                </div>
                </div>
                </div>})))

;;; ANSWERS

; (define-struct answer (html css show get))

(define number-answer 
  (make-answer 
   @stringify{ <foo>...</foo> }
   @stringify{ {foo: bar;} }
   (lambda () 
     (show-answer-field 1)
     (show-answer-label 1))
   (lambda () (string->number
               (string-trim-both
                (get-answer-field 1))))))

(define three-number-answer 
  (make-answer 
   @stringify{ <foo>...</foo> }
   @stringify{ {foo: bar;} }
   (lambda () 
     (for-each show-answer-field '(1 2 3))
     (for-each show-answer-label '(1 2 3)))
   (lambda () 
     (map (lambda (n)
            (string->number
             (string-trim-both
              (get-answer-field n))))
          '(1 2 3)))))

(define (insert-at i x xs)
  (if (zero? i) 
      (cons x xs)
      (cons (car xs)
            (insert-at (- i 1) x (cdr xs)))))

(define (multiple-choice-answer generate-correct-choice generate-wrong-choices)
  (define (shuffle xs)
    (define (loop xs ys)
      (if (null? xs)
          ys
          (loop (cdr xs)
                (insert-at (random (+ (length ys) 1)) (car xs) ys))))
    (loop xs '()))
  (let* ([correct-index "uninialized"]
         [choices "uninialized"]
         [wrong-choices "uninialized"])
    (let ()
      (make-answer
       @stringify{ <foo>...</foo> }
       @stringify{ {foo: bar;} }
       (lambda ()
         (set! wrong-choices (generate-wrong-choices))
         (let ([indices (build-list (+ 1 (length wrong-choices)) values)])
           (set! correct-index (random (length wrong-choices)))
           (set! choices (insert-at correct-index (generate-correct-choice) (shuffle wrong-choices)))
           (for-each (lambda (i c)
                       (show-multiple-choice-radio-button i)
                       (change-multiple-choice-text i (list-ref choices i)))
                     indices choices)
           (run-mathjax)
           (void)))
       (lambda ()
         (= correct-index (get-multiple-choice)))))))

;;
;;; EXERCISES
;;;

(define (antiderivative-of-power-exercise)
  (define exercises
    ; f F list-of-wrong-choices
    '(("1"      "x"                ("0"    "2x"    "\\frac{1}{2}x"   "\\frac{1}{2}x^2"))
      ("x"      "\\frac{1}{2}x^2"  ("1"    "x^2"   "\\frac{1}{2}x"   "\\frac{1}{2}x^1"))
      ("x^2"    "\\frac{1}{3}x^3"  ("2x"   "x^3"   "\\frac{1}{2}x^2" "\\frac{1}{3}x^2"))
      ("x^3"    "\\frac{1}{4}x^4"  ("3x^2" "x^4"   "\\frac{1}{3}x^3" "\\frac{1}{4}x^3"))
      ("x^4"    "\\frac{1}{5}x^5"  ("4x^3" "x^5"   "\\frac{1}{4}x^4" "\\frac{1}{5}x^4"))
      ("x^5"    "\\frac{1}{6}x^6"  ("5x^4" "x^6"   "\\frac{1}{5}x^5" "\\frac{1}{6}x^5"))
      ("x^6"    "\\frac{1}{7}x^7"  ("6x^5" "x^7"   "\\frac{1}{6}x^6" "\\frac{1}{7}x^6"))
      ("x^{-2}" "-x^{-1}=\\frac{-1}{x}"  ("x^{-1}=\\frac{1}{x}" "\\frac{1}{-3}x^{-3}" "x^{-1}"  "-2x"))
      ("x^{-3}" "\\frac{-1}{2}x^{-2}"  ("\\frac{1}{-4}x^{-4}=\\frac{-4}{x^4}" 
                                        "\\frac{1}{-3}x^{-3}=\\frac{-1}{3x^{-3}}" 
                                        "-3x^{-2}"  "-3x^{-4}"))
      ("x^{-1} = \\frac{1}{x}"  "\\ln(x)"  ("\\log(x)" "\\frac{1}{-2}x^{-2}" "x^{-2}" "1"))))
  
  (define f "uninitialized")
  (define F "uninitialized")
  (define wrongs "uninitialized")
  
  (define (new-problem!)
    (let ([ex (list-ref exercises (random (length exercises)))])
      (set! f (car ex))
      (set! F (cadr ex))
      (set! wrongs (caddr ex))))
  
  (define (problem-description)
    @stringify{<div>What is the antiderivative of various powers.</div>
               <div> $$@f  ?$$ </div>})
  
  (define (hints)
    (list @stringify{The antiderivative of $@|f|$ is $@|F|$}))
  
  (define (check-answer ans)
    ans)
  
  (define ($wrap str)
    (string-append "$" str "$"))
  
  (new-problem!)
  (make-exercise "Antideriviative" 
                 "Antiderivatives of powers."
                 new-problem! problem-description hints check-answer 
                 (multiple-choice-answer (λ() ($wrap F)) (λ () (map $wrap wrongs)))))

(define (addition-exercise)
  ; The exercises are of the form x+y where x and y are small integers.
  ; The student must answer correctly, before a new problem is generated.
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
                 new-problem! problem-description hints check-answer number-answer))

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
    (cond
      [(= n 0) (list @stringify{The power $a^0$ is defined to be $1$.}
                     @stringify{$$@|a|^0 = 1.$$})]
      [else    (list @stringify{The power $a^n$ is $a$ multiplied with itself $n$ times.}
                     @stringify{$$a^@|n| = a @(string-append-n "\\cdot a" (sub1 n)).$$}
                     @stringify{$$@|a|^@|n| = @a @(string-append-n @stringify{\cdot @|a|} (sub1 n)).$$}
                     @stringify{$$@|a|^@|n| = @(expt a n).$$})]))

  (define (check-answer ans)
    (= (expt a n) ans))
  
  (new-problem!)
  (make-exercise "Powers with natural exponents" 
                 "Powers with natural exponents."
                 new-problem! problem-description hints check-answer number-answer))

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
          @stringify{$$@|a|^{@n} = \frac{1}{@|a|^{@(- n)}} = \frac{1}{@|a|@(string-append-n 
                                                                            @stringify{\cdot @|a|} (sub1 (- n)))}. $$}
          @stringify{$$@|a|^{@n} = \frac{1}{@(expt a (- n))}.$$ Enter the result as $1/@(expt a (- n))$.}))
  
  (define (check-answer ans)
    (= (expt a n) ans))
  
  (new-problem!)
  (make-exercise "Powers with negative exponents" 
                 "Powers with natural exponents."
                 new-problem! problem-description hints check-answer number-answer))

(define (squaring-binomial-exercise)
  ; (ax+b)^2 = a^2 x^2 + 2ab x + b^2  
  (define a 1)
  (define b 2)
  ; depends on a and b:
  (define c2 (* a a))
  (define c1 (* 2 a b))
  (define c0 (* b b))
  
  (define (new-problem!)
    (set! a (random-in-interval 2 6))
    (when (= a 0) (new-problem!))
    (set! b (random-in-interval 2 6))
    (set! c2 (* a a))
    (set! c1 (* 2 a b))
    (set! c0 (* b b)))
  
  (define (problem-description)
    @stringify{<div>Rewrite to the form $a\cdot x^2 + b\cdot x + c$.</div>
               <div>$$( @|a|\cdot x + @|b|)^2 = ?$$</div>
               })
  
  (define (hints)
    (list @stringify{
                     First use the rule:  $(s+t)^2=s^2+2\cdot s\cdot t+t^2$ <br/>
                     Second use the rule: $(a\cdot s)^2=a^2\cdot s^2$}
          @stringify{ $$(@|a|\cdot x + @|b|)^2 = (@|a|\cdot x)^2 + 2\cdot(@|a|x)\cdot @|b| + @|b|^2 $$}
          @stringify{ $$ = @|a|^2\cdot x^2 + 2\cdot(@|a|x)\cdot @|b| + @|b|^2$$}
          @stringify{ $$ = @|c2|x^2 + @|c1|x + @|c0|$$}
          ))
  
  (define (check-answer ans)
    (equal? ans (list c2 c1 c0)))
  
  (new-problem!)
  (make-exercise "Square of binomial" 
                 "Square a binomial."
                 new-problem! problem-description hints check-answer 
                 three-number-answer))

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
      (define answer-type (exercise-answer-type (list-ref all current)))
      (make-exercise title summary new-problem! problem-description hints 
                     check-answer answer-type))))

(define (power-all-kinds-exercise)
  (combine-exercises "Powers" "Combines all power exercises" 
                     power1-exercise power2-exercise))

(define current-exercise 
  #;(power-all-kinds-exercise)
  ;(squaring-binomial-exercise)
  (antiderivative-of-power-exercise)
  )


;;; START

(void (js-eval "storeProxy();")
      ((exercise-new-problem! current-exercise))
      (generate-body)
      (update-exercise-title)
      (hide-answer-inputs-and-labels)
      (hide-multiple-choice-radio-buttons)
      (on-answer-button)
      (clear-answer-input)
      (hide-answer-inputs-and-labels)
      ((answer-show (exercise-answer-type current-exercise)))
      (update-streak-bar)
      (run-mathjax))
