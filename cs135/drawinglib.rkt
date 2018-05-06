#lang racket

;; This library relies on a couple of additional libraries that are
;; built in to Racket.  Go out and get them.
(require 2htdp/image)
(require lang/posn)

;; Tell Racket which parts of this file should be visible to the outside
;; world.  You can ignore this.
(provide draw-picture
         (struct-out circle135)
         (struct-out square135)

         black white red blue green yellow

         save-image)

;; Data definitions for this question, as given in the assignment handout.
;; You do not need to copy this information into drawing.rkt, it should
;; already be available there.  You should ignore the magic annotation
;; "#:transparent" at the end of the structure definitions.

;; A Colour is a (list Num Num Num)
;; requires: the three numbers are between 0 and 255 inclusive.

(define-struct circle135 (centre radius fill) #:transparent)
;; A Circle135 is a (make-circle135 Posn Num Colour)
;; requires: radius >= 0

(define-struct square135 (corner side fill) #:transparent)
;; A Square135 is a (make-square135 Posn Num Colour)
;; requires: side >= 0

;; A Drawing is a (listof (anyof Circle135 Square135))

;; Some handy colour constants in case you want to use them for testing.
(define black '(0 0 0))
(define white '(255 255 255))
(define red '(255 0 0))
(define yellow '(255 255 0))
(define green '(0 255 0))
(define blue '(0 0 255))

;; (draw-picture lst w h) constructs a raster image with dimensions (w x h),
;;   based on the circles and squares in lst.
;; draw-picture: Drawing Nat Nat -> Image

(define (draw-picture lst w h)
  ;; This function uses a number of language features that we're not
  ;; ready to talk about yet in this course, and we won't get into
  ;; the image drawing tools themselves.  We don't want you to look
  ;; too hard at how this function works internally, and so we've left
  ;; it a bit cryptic on purpose.
  (local
    [(define (gc l) (apply make-color (map round l)))
     
     (define (as sq img)
       (local
         [(define tl (square135-corner sq))
          (define sd (square135-side sq))
          (define fl (square135-fill sq))
          (define fc (gc fl))]
         (place-image 
          (square sd "solid" fc)
          (+ (posn-x tl) (/ sd 2)) (+ (posn-y tl) (/ sd 2)) img)))

     (define (ac ci img)
       (local
         [(define ce (circle135-centre ci))
          (define ra (circle135-radius ci))
          (define fl (circle135-fill ci))
          (define fc (gc fl))]
         (place-image 
          (circle ra "solid" fc)
          (posn-x ce) (posn-y ce) img)))

     (define (loop lst acc)
       (cond
         [(empty? lst) acc]
         [(circle135? (first lst))
          (loop (rest lst) (ac (first lst) acc))]
         [(square135? (first lst))
          (loop (rest lst) (as (first lst) acc))]))]
    (loop lst (rectangle w h "solid" "white"))))

;; It's not really practical to test draw-picture, given that the function
;; produces an actual image, not data we can compare against.
