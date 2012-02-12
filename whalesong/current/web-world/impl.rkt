#lang s-exp "../lang/js/js.rkt"


;; Make sure the resource library is loaded, as well as the event structure library.
(require "../resource.rkt" "event.rkt")

(declare-implementation
 #:racket "racket-impl.rkt"
 #:javascript ("js-tree-cursor.js"
               "js-impl.js")
 #:provided-values (big-bang

                    ;; initial view 
                    initial-view

                    ;; stop-when handler
                    stop-when

                    ;; clock tick handler
                    on-tick

                    ;; location changes
                    on-mock-location-change

                    ;; location changes (for real!)
                    on-location-change
                    
                    ;; draw and update the view
                    to-draw

                    ;; helper: open an element as an output port.
                    open-output-element                    
                    
                    ;; coerse to view
                    ->view

                    view?
                    
                    view-focus?
                    view-focus
                    
                    view-left?
                    view-left

                    view-right?
                    view-right

                    view-up?
                    view-up
                    
                    view-down?
                    view-down

                    view-forward?
                    view-forward
                    view-backward?
                    view-backward
                    
                    view-text
                    update-view-text

                    view-bind

                    view-show
                    view-hide
                    
                    view-attr
                    view-has-attr?
                    update-view-attr
                    remove-view-attr

                    view-css
                    update-view-css
                    
                    view-id
                    
                    view-form-value
                    update-view-form-value

                    view-append-child
                    view-insert-right
                    view-insert-left
                    view-remove                    
                    
                    xexp?
                    xexp->dom

                    view->xexp
                    
                    ))
