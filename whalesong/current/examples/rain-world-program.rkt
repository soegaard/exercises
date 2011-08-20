#lang planet dyoo/whalesong

(require (planet dyoo/whalesong/world)
         (planet dyoo/whalesong/image)
         (planet dyoo/whalesong/js))

;; Occupy the whole screen.
(void (call-method body "css" "margin" 0))
(void (call-method body "css" "padding" 0))
(void (call-method body "css" "overflow" "hidden"))


;; Rain falls down the screen.
(define GRAVITY-FACTOR 1)



(define-struct posn (x y))


;; A drop particle describes where it is on screen, what color it is, and
;; how large it is.
(define-struct drop (posn velocity color size)) 

;; random-drop-particle: drop
;; Generates a random particle.
(define (random-drop)
  (make-drop (make-posn (random (viewport-width)) 0)
             (+ 5 (random 10)) ;; Get it falling at some random velocity
             (random-choice (list "gray" "darkgray"
                                  "white" "blue" 
                                  "lightblue"
                                  "darkblue"))
             (random 10) ;; with some random size
             ))

;; random-choice: (listof X) -> X
;; Picks a random element of elts.
(define (random-choice elts)
  (list-ref elts (random (length elts))))



;; The world consists of all of the drops in the sky.
(define-struct world (sky ;; listof drop 
                          ))



(define (my-filter f l)
  (cond
   [(null? l)
    '()]
   [(f (car l))
    (cons (car l)
          (my-filter f (cdr l)))]
   [else
    (my-filter f (cdr l))]))


;; tick: world -> world
(define (tick w)
  (make-world 
   (my-filter not-on-floor?
           (map drop-descend (cons (random-drop)
                                   (cons (random-drop)
                                         (world-sky w)))))))


;; drop-descend: drop -> drop
;; Makes the drops descend.
(define (drop-descend a-drop)
  (cond
    [(> (posn-y (drop-posn a-drop)) (viewport-height))
     a-drop]
    [else
     (make-drop (posn-descend (drop-posn a-drop) (drop-velocity a-drop))
                (+ GRAVITY-FACTOR (drop-velocity a-drop))
                (drop-color a-drop)
                (drop-size a-drop))]))


;; posn-descend: posn number -> posn
(define (posn-descend a-posn n)
  (make-posn (posn-x a-posn)
             (+ n (posn-y a-posn))))


;; on-floor?: drop -> boolean
;; Produces true if the drop has fallen to the floor.
(define (on-floor? a-drop)
  (> (posn-y (drop-posn a-drop))
     (viewport-height)))

(define (not-on-floor? a-drop) (not (on-floor? a-drop)))

;; make-drop-image: color number -> drop
;; Creates an image of the drop particle.
(define (make-drop-image color size)
  (circle size "solid" color))


;; place-drop: drop scene -> scene
(define (place-drop a-drop a-scene)
  (place-image (make-drop-image (drop-color a-drop)
                                (drop-size a-drop))
               (posn-x (drop-posn a-drop))
               (posn-y (drop-posn a-drop))
               a-scene))



(define (my-foldl f acc lst)
  (cond
   [(null? lst)
    acc]
   [else
    (my-foldl f
              (f (car lst) acc)
              (cdr lst))]))
                   

;; draw: world -> scene
(define (draw w)
  (my-foldl place-drop (empty-scene (viewport-width) (viewport-height)) (world-sky w)))



(big-bang (make-world '())
          (to-draw draw)
          (on-tick tick))