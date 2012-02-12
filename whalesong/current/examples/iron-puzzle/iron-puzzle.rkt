#lang planet dyoo/whalesong/base
(require (planet dyoo/whalesong/resource)
         (planet dyoo/whalesong/image))

;; The Iron Image Puzzle
;; Part of the Nifty Assignments page by Nick Parlante.
;; http://nifty.stanford.edu/2011/parlante-image-puzzle/

;; To run this program locally on your machine under Google Chrome, you'll
;; probably need to use --allow-file-access-from-files to get around
;; the same-origin policy.  The program should run normally if served on
;; a web server

(define-resource iron-puzzle.png)

 
;; First, grab the image by its url.
(define distorted-image iron-puzzle.png)


distorted-image              
(image-width distorted-image)
(image-height distorted-image)

;; We will want to decompose the image into its individual pixels.  We can use
;; image->color-list to do this.
;; To make experimenting with the pixels an easy thing to do, let?s write
;; a function that will let us ?map? across the pixels of an image
;; to get another image.
(define (image-transform an-image a-pixel-transform)
  (local [(define colors (image->color-list an-image))
          (define new-colors (map a-pixel-transform colors))]
    (color-list->image new-colors
                       (image-width an-image)
                       (image-height an-image)
                       0
                       0)))


;; With image-transform in hand, we can apply a filter,
;; such as zeroing out the blue and green components like this:
(define (zero-blue-green a-color)
  (make-color (color-red a-color)
              0
              0))
(define dark-image
  (image-transform distorted-image zero-blue-green))

dark-image


;; The image is still a bit darkened.  Let?s bump up the red component
;; by a factor of ten.
(define (red*10 a-color)
  (make-color (* (color-red a-color) 10)
              (color-green a-color)
              (color-blue a-color)))
(define red-eye-image
  (image-transform dark-image red*10))

;; And now we should be able to look at red-eye-image and recognize
;; the landmark.

red-eye-image
(image-width red-eye-image)
(image-height red-eye-image)
