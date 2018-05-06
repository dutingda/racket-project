;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname drawing) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YOUR NAME  (ID#)
;; CS135 Fall 2017
;; Assignment 05, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the data definitions and functions in
;; the file drawinglib.rkt.
(require "drawinglib.rkt")

;; A demonstration of the drawing in the assignment.
(define samplepic (list
                   (make-square135 (make-posn 0 0) 50 '(255 0 0))
                   (make-square135 (make-posn 50 50) 50 '(0 0 255))
                   (make-circle135 (make-posn 50 50) 25 '(0 255 0))))

;; Type this line into the interactions window to see the picture:
;; (draw-picture samplepic 100 100)

;; --------------------------
;; Place your functions here!
;; --------------------------

;; (a)
(define example-drawing
  (list (make-square135 (make-posn 0 0) 100 '(0 0 0))
        (make-square135 (make-posn 0 100) 100 '(0 0 0))
        (make-square135 (make-posn 12.5 25) 75 '(128 128 128))
        (make-square135 (make-posn 12.5 100) 75 '(128 128 128))
        (make-circle135 (make-posn 50 187.5) 10 '(128 128 128))
        (make-circle135 (make-posn 50 187.5) 7 '(0 0 0))
        (make-circle135 (make-posn 72.5 40) 4 '(255 255 255))
        (make-square135 (make-posn 68.5 50) 8 '(255 255 255))
        (make-circle135 (make-posn 72.5 50) 4 '(255 255 255))
        (make-circle135 (make-posn 72.5 58) 4 '(255 255 255))
        (make-circle135 (make-posn 50 12.5) 5 '(128 128 128))))
        

;; (b)
;; (cull drawing m n) produces a new drawing containing the first m squares and the
;;   first n circles in the orginal "drawing"
;; (cull Drawing Nat Nat -> Drawing)
;; Examples:
(check-expect (cull samplepic 1 1) (list (make-square135 (make-posn 0 0) 50 '(255 0 0))
                                         (make-circle135 (make-posn 50 50) 25 '(0 255 0))))

(define (cull drawing m n)
  (cond
    [(or (empty? drawing)
         (and (= m 0) (= n 0))) empty]
    [(and (> m 0)
          (square135? (first drawing)))
     (cons (first drawing)
           (cull (rest drawing) (sub1 m) n))]
    [(and (> n 0)
          (circle135? (first drawing)))
     (cons (first drawing)
           (cull (rest drawing) m (sub1 n)))]
    [else
     (cull (rest drawing) m n)]))


;; Tests:
(check-expect (cull (list (make-square135 (make-posn 0 0) 50 '(255 0 0))
                          (make-circle135 (make-posn 50 50) 25 '(0 255 0))
                          (make-square135 (make-posn 0 3) 50 '(255 124 0))
                          (make-circle135 (make-posn 0 3) 50 '(255 124 0))) 0 3)
              (list (make-circle135 (make-posn 50 50) 25 '(0 255 0))
                    (make-circle135 (make-posn 0 3) 50 '(255 124 0))))
(check-expect (cull empty 10 30) empty)
(check-expect (cull (list (make-square135 (make-posn 0 0) 50 '(255 0 0))) 0 1) empty)

;; (c)
;; (draw-circle base-radius n) produces a n alternating red and white discs centred
;;   on the point (100, 100) with the interval of base-radius
;; (draw-circle: Nat Nat -> Drawgin)

(define (draw-circle base-radius n)
  (cond
    [(= n 0) empty]
    [(even? n)
     (cons (make-circle135 (make-posn 100 100) (* base-radius n) '(255 0 0))
           (draw-circle base-radius(- n 1)))]
    [(odd? n)
     (cons (make-circle135 (make-posn 100 100) (* base-radius n) '(255 255 255))
           (draw-circle base-radius (- n 1)))]))


;; (bullseye n) produces a Drawing containing n alternating red and white discs centred
;;   on the point (100, 100)
;; (bullseye: Nat -> Drawing)
;; Examples:
              

(define (bullseye n)
  (draw-circle (/ 100 n) n))
  
     
;; (d)
;; (checkerboard length n c1 c2) produces a checkerboard with length that alternates between
;;   two tile shapes sqauares filled with colour c1 and circles filled with colour c2
;; (checkerboard Nat Colour Colour -> Drawing)


(define (checkerboard-help length n c1 c2)
  (cond
    [(= length 0) empty]
    [(= 0 n) (cons (make-square135 (make-posn 0 0) 10 c1) empty)]
    [(even? (+ (floor (/ n length)) (remainder n length)))
     (cons (make-square135 (make-posn (* 10 (remainder n length))
                                      (* 10 (floor (/ n length)))) 10 c1)
           (checkerboard-help length (sub1 n) c1 c2))]
    [(odd? (+ (floor (/ n length)) (remainder n length)))
     (cons (make-circle135 (make-posn (+ 5 (* 10 (remainder n length)))
                                      (+ 5 (* 10 (floor (/ n length))))) 5 c2)
           (checkerboard-help length (sub1 n) c1 c2))]))

(define (checkerboard n c1 c2)
  (checkerboard-help n (- (sqr n) 1) c1 c2))

                                         
                                      
            

                    
 
        