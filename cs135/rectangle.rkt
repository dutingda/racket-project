;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rectangle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 09, Question 02
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "rectanglelib.rkt")

(define-struct cell (num used?))
;; A Cell is a (make-cell Nat Bool)

;; A Grid is a (listof (listof Cell))
;; requires: the grid contains a non-empty list of non-empty lists,
;;  all the same length.

(define-struct rect (x y w h))
;; A Rect is a (make-rect Nat Nat Nat Nat)

(define-struct state (grid rects))
;; A State is a (make-state Grid (listof Rect))


;; Here are a couple of constants that can be used to define
;; the puzzle in the assignment, and a random larger puzzle.

(define puzz '((0 0 0 0 0 5 0)
               (0 0 0 0 0 2 2)
               (0 3 0 6 3 2 0)
               (4 0 0 0 0 0 0)
               (0 0 0 4 0 4 0)
               (2 0 6 0 2 4 0)
               (0 0 0 0 0 0 0)))

(define big-puzz '((4 0 7 0 0 0 0 0 0 0 0 21 0)
                   (0 3 2 0 0 0 0 0 0 0 0 0 2)
                   (0 0 0 0 0 0 0 2 3 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 5)
                   (0 2 0 0 0 0 0 4 0 0 0 0 0)
                   (0 0 3 0 0 0 0 0 0 0 0 0 0)
                   (3 0 0 0 0 5 2 4 0 0 0 0 0)
                   (0 0 0 0 0 2 0 6 0 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 24 0)
                   (0 0 0 0 4 0 4 0 0 0 4 0 0)
                   (0 0 3 0 0 0 0 0 0 0 8 0 2)))
(define test-state
  (make-state (list (list (make-cell 0 true)
                          (make-cell 0 true)
                          (make-cell 0 true)
                          (make-cell 2 true)
                          (make-cell 0 true)
                          (make-cell 2 true)
                          (make-cell 0 true))
                    (list (make-cell 0 true)
                          (make-cell 0 true)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 4 false)
                          (make-cell 0 true))
                    (list (make-cell 0 true)
                          (make-cell 3 true)
                          (make-cell 6 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 true))
                    (list (make-cell 4 true)
                          (make-cell 2 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 4 false)
                          (make-cell 0 true))
                    (list (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 2 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 true))
                    (list (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 10 false)
                          (make-cell 0 false)
                          (make-cell 7 true))
                    (list (make-cell 3 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 0 true)))
              (list (make-rect 0 0 1 4)
                    (make-rect 0 1 1 3)
                    (make-rect 0 2 2 1)
                    (make-rect 0 4 1 4)
                    (make-rect 0 6 1 7))))
                         
                         

;; (a)
;; (map2d f lolov) Produces a new list of lists in which f has been plied to every
;;   element of the lolov.
;; map2d: (X -> Y) (listof (listof X)) -> (listof (listof Y))
;; Example:
(check-expect (map2d add1 '((3 4 5) (10 9 8))) '((4 5 6) (11 10 9)))

(define (map2d f lolov)
  (map (lambda (x) (map f x)) lolov))

;; Tests:
(check-expect (map2d posn-x (list (list (make-posn 1 'a) (make-posn 2 'b))
                                  (list (make-posn 3 'c) (make-posn 4 'd))))
              '((1 2) (3 4)))
(check-expect (map2d string-length '(("ab" "sbc" "fdfd") ("as" "") ("afe")))
              '((2 3 4) (2 0) (3)))


;; (b)
;; (construct-puzzle lolon) Produces a State representing the initial state of a puzzle
;;   which is the lolon
;; construct-puzzle: (listof (listof Nat)) -> State
;; Example:
(check-expect (construct-puzzle (list (list 1 2 0) (list 0 3 0) (list 4 0 1)))
              (make-state (list (list (make-cell 1 false)
                                      (make-cell 2 false)
                                      (make-cell 0 false))
                                (list (make-cell 0 false)
                                      (make-cell 3 false)
                                      (make-cell 0 false))
                                (list (make-cell 4 false)
                                      (make-cell 0 false)
                                      (make-cell 1 false))) empty))


(define (construct-puzzle lolon)
  (make-state (map2d (lambda (x) (make-cell x false)) lolon) empty))


;; Tests:
(check-expect (construct-puzzle (list (list 2 0) (list 0 1)))
              (make-state (list (list (make-cell 2 false)
                                      (make-cell 0 false))
                                (list (make-cell 0 false)
                                      (make-cell 1 false))) empty))
(check-expect (construct-puzzle (list (list 1 2 3 4 5)
                                      (list 0 0 0 0 0)
                                      (list 0 0 0 0 0)
                                      (list 0 0 0 0 0)
                                      (list 4 3 2 1 0)))
              (make-state (list (list (make-cell 1 false)
                                      (make-cell 2 false)
                                      (make-cell 3 false)
                                      (make-cell 4 false)
                                      (make-cell 5 false))
                                (list (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false))
                                (list (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false))
                                (list (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false)
                                      (make-cell 0 false))
                                (list (make-cell 4 false)
                                      (make-cell 3 false)
                                      (make-cell 2 false)
                                      (make-cell 1 false)
                                      (make-cell 0 false))) empty))


;; (c)
;; (solved? sta) Produces a boolean value tht indicates whether the puzzle described by
;;   the sta is fully solved.
;; solved?: State -> Bool
;; Example:
(check-expect (solved? (make-state (list (list (make-cell 2 true)
                                               (make-cell 0 true))
                                         (list (make-cell 0 true)
                                               (make-cell 1 true)))
                                   (list (make-rect 1 2 1 2)
                                         (make-rect 3 1 3 1)))) true)
              

(define (solved? sta)
  (not
   (member?
    false
    (foldr append empty (map2d (lambda (x) (cell-used? x)) (state-grid sta))))))


;; Tests:
(check-expect (solved? (make-state
                        (list
                         (list
                          (make-cell 0 false)
                          (make-cell 2 false)
                          (make-cell 0 false)
                          (make-cell 1 false))
                         (list
                          (make-cell 0 false)
                          (make-cell 0 false)
                          (make-cell 2 false)
                          (make-cell 0 false))) empty)) false)


;; (d)
;; (get-first-unused grid) Finds the topmost, leftmost cell in the grid that isn't
;;    marked as used and produces a (list Nat Nat) containing the x and y coordinates
;;    of the first unused cell
;; get-first-unused: Grid -> Cell
;; Example:
(check-expect (get-first-unused (list (list (make-cell 2 true)
                                            (make-cell 0 true))
                                      (list (make-cell 0 false)
                                            (make-cell 1 true)))) (list 0 1))


(define (get-first-unused grid)
  (local
    [(define a (foldr append empty (map2d (lambda (x) (cell-used? x)) grid)))
     (define b (length grid))
     (define c
       (second
        (first
         (filter (lambda (x) (not (first x)))
                 (foldr (lambda (x y) (cons (list x (sub1 (second (first y)))) y))
                        (list (list true (sqr b))) a)))))]
    (list (remainder c b) (quotient c b))))


;; Tests:
(check-expect (get-first-unused
               (list
                (list
                 (make-cell 0 false)
                 (make-cell 2 false)
                 (make-cell 0 false)
                 (make-cell 1 false))
                (list
                 (make-cell 0 false)
                 (make-cell 0 false)
                 (make-cell 2 false)
                 (make-cell 0 false)))) (list 0 0))
(check-expect (get-first-unused (list
                                 (list
                                  (make-cell 0 true)
                                  (make-cell 1 true))
                                 (list
                                  (make-cell 0 true)
                                  (make-cell 0 false)))) (list 1 1))
(check-expect (get-first-unused (list (list (make-cell 1 true)
                                            (make-cell 2 true)
                                            (make-cell 3 true)
                                            (make-cell 4 true)
                                            (make-cell 5 true))
                                      (list (make-cell 0 true)
                                            (make-cell 0 true)
                                            (make-cell 0 true)
                                            (make-cell 0 true)
                                            (make-cell 0 true))
                                      (list (make-cell 0 true)
                                            (make-cell 0 true)
                                            (make-cell 0 true)
                                            (make-cell 0 true)
                                            (make-cell 0 false))
                                      (list (make-cell 0 false)
                                            (make-cell 0 false)
                                            (make-cell 0 false)
                                            (make-cell 0 false)
                                            (make-cell 0 false))
                                      (list (make-cell 4 false)
                                            (make-cell 3 false)
                                            (make-cell 2 false)
                                            (make-cell 1 false)
                                            (make-cell 0 false)))) (list 4 2))

;;(unused-cells
;; (construct
;;  (get-first-unused (state-grid test-state))
;;  (length (state-grid test-state))) (state-grid test-state) empty) 

;; (e)       
;; (neighbours sta) Produces a lit of new states that miht legitimately follow from the
;;   follow from the given state after adding a single new rectangle. If no legal
;;   rectangles can be added from the current state, produce the empty list
;; neighbours: State -> (listof State)
;; Example:
(check-expect (neighbours test-state)
              (list
               (make-state
                (list
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 4 true)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 3 true)
                  (make-cell 6 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 4 true)
                  (make-cell 2 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 4 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 2 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 10 false)
                  (make-cell 0 false)
                  (make-cell 7 true))
                 (list
                  (make-cell 3 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true)))
                (list
                 (make-rect 2 1 4 1)
                 (make-rect 0 0 1 4)
                 (make-rect 0 1 1 3)
                 (make-rect 0 2 2 1)
                 (make-rect 0 4 1 4)
                 (make-rect 0 6 1 7)))
               (make-state
                (list
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 4 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 3 true)
                  (make-cell 6 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 4 true)
                  (make-cell 2 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 4 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 2 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 10 false)
                  (make-cell 0 false)
                  (make-cell 7 true))
                 (list
                  (make-cell 3 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true)))
                (list
                 (make-rect 2 1 3 2)
                 (make-rect 0 0 1 4)
                 (make-rect 0 1 1 3)
                 (make-rect 0 2 2 1)
                 (make-rect 0 4 1 4)
                 (make-rect 0 6 1 7)))
               (make-state
                (list
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 4 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 3 true)
                  (make-cell 6 true)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 4 true)
                  (make-cell 2 false)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 4 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 2 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 10 false)
                  (make-cell 0 false)
                  (make-cell 7 true))
                 (list
                  (make-cell 3 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true)))
                (list
                 (make-rect 2 1 2 3)
                 (make-rect 0 0 1 4)
                 (make-rect 0 1 1 3)
                 (make-rect 0 2 2 1)
                 (make-rect 0 4 1 4)
                 (make-rect 0 6 1 7)))
               (make-state
                (list
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true)
                  (make-cell 2 true)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 4 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 true)
                  (make-cell 3 true)
                  (make-cell 6 true)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 4 true)
                  (make-cell 2 false)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 4 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true)
                  (make-cell 2 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true))
                 (list
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 10 false)
                  (make-cell 0 false)
                  (make-cell 7 true))
                 (list
                  (make-cell 3 false)
                  (make-cell 0 false)
                  (make-cell 0 true)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 false)
                  (make-cell 0 true)))
                (list
                 (make-rect 2 1 1 6)
                 (make-rect 0 0 1 4)
                 (make-rect 0 1 1 3)
                 (make-rect 0 2 2 1)
                 (make-rect 0 4 1 4)
                 (make-rect 0 6 1 7)))))

(define (neighbours sta)
  (local    
    [;; (construct coo len) Produces the left-down side of all
     ;;    the first-unused rectangle centered rectangles in the grid
     ;; construct: (list Nat Nat) Nat -> (list Rect)
     (define (construct coo len)
       (foldr append empty
              (build-list (- len (second coo))
                          (lambda (y)
                            (build-list (- len (first coo))
                                        (lambda (x)
                                          (make-rect (first coo) (second coo)
                                                     (add1 x) (add1 y))))))))
     ;; (unused-cells lor grid acc) Produces the all rectangles that is not
     ;;   used in the coustruct list of rectangles in the grid with the help
     ;;   of accummlators
     ;; usused-cells: (listof Rect) Grid Nat -> (listof Rect)
     (define (unused-cells lor grid acc)
       (cond
         [(empty? lor) acc]
         [(cell-used? (list-ref 
                       (list-ref grid (sub1 (+ (rect-y (first lor)) (rect-h (first lor)))))
                       (sub1 (+ (rect-x (first lor)) (rect-w (first lor))))))
          (unused-cells (rest lor) grid acc)]
         [else
          (unused-cells (rest lor) grid (cons (first lor) acc))]))
     ;; (single-with-same-area lor grid acc) Produces the rectangles that is not
     ;;    contained another number and the area is the same as the single number
     ;; single-with-same-area: (listof Rect) Grid Nat -> (listof Rect)
     (define (single-with-same-area lor grid acc)
       (local
         [;; (f rec) Produces all the posn which the position has the non-zero cells
          ;;    with rec
          ;; f: Rect -> (listof Posn)
          (define (f rec)
            (filter
             (lambda (t) (not (zero? (cell-num (list-ref (list-ref grid (posn-y t)) (posn-x t))))))
             (foldr append empty
                    (build-list (rect-h rec)
                                (lambda (y)
                                  (build-list (rect-w rec)
                                              (lambda (x)
                                                (make-posn (+ (rect-x rec) x)
                                                           (+ (rect-y rec) y)))))))))]
         (cond
           [(empty? lor) acc]
           [else
            (local
              [(define l (f (first lor)))]
              (cond
                [(and (= (length l) 1) (= (cell-num (list-ref (list-ref grid (posn-y (first l)))
                                                              (posn-x (first l))))
                                          (* (rect-w (first lor))
                                             (rect-h (first lor)))))

                 (single-with-same-area (rest lor) grid (cons (first lor) acc))]
                [else
                 (single-with-same-area (rest lor) grid acc)]))])))
     ;; (mark-used rec grid acc) marks all the Cell in the rectangle to be used, which
     ;;    is true
     ;; mark-used: Rect Grid -> Grid
     (define (mark-used rec grid acc)
       (local
         [;; (f lst ac) Transfer all the rectangles of one row within the range to be used
          ;; f: (listof Cell) Nat -> (listof Cell)
          (define (f lst ac)
            (cond
              [(empty? lst) empty]
              [(or (< ac (rect-x rec)) (>= ac (+ (rect-w rec) (rect-x rec))))
               (cons (first lst) (f (rest lst) (add1 ac)))]
              [else
               (cons (make-cell (cell-num (first lst)) true)
                     (f (rest lst) (add1 ac)))]))]
         (cond
           [(empty? grid) empty]
           [(or (< acc (rect-y rec)) (>= acc (+ (rect-h rec) (rect-y rec))))
            (cons (first grid) (mark-used rec (rest grid) (add1 acc)))]
           [else
            (cons (f (first grid) 0) (mark-used rec (rest grid) (add1 acc)))])))
     (define grid (state-grid sta))
     (define rects-existed (state-rects sta))
     (define lor1
       (construct (get-first-unused grid) (length grid)))
     (define lor2
       (unused-cells lor1 grid empty))
     (define lor3
       (single-with-same-area lor2 grid empty))]
    (map (lambda (x) (make-state (mark-used x grid 0) (cons x rects-existed))) lor3)))


;; Tests:
(check-expect (neighbours
               (make-state (list (list (make-cell 1 false)
                                       (make-cell 2 false)
                                       (make-cell 3 false)
                                       (make-cell 4 false)
                                       (make-cell 5 false))
                                 (list (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false))
                                 (list (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false))
                                 (list (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false)
                                       (make-cell 0 false))
                                 (list (make-cell 4 false)
                                       (make-cell 3 false)
                                       (make-cell 2 false)
                                       (make-cell 1 false)
                                       (make-cell 0 false))) empty))
              (list
               (make-state
                (list
                 (list (make-cell 1 true) (make-cell 2 false)
                       (make-cell 3 false) (make-cell 4 false)
                       (make-cell 5 false))
                 (list (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false))
                 (list (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false))
                 (list (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false) (make-cell 0 false)
                       (make-cell 0 false))
                 (list (make-cell 4 false) (make-cell 3 false)
                       (make-cell 2 false) (make-cell 1 false)
                       (make-cell 0 false)))
                (list (make-rect 0 0 1 1)))))
(check-expect (neighbours (make-state (list
                                      (list
                                       (make-cell 0 true)
                                       (make-cell 1 true))
                                      (list
                                       (make-cell 0 false)
                                       (make-cell 0 false))) (list (make-rect 0 0 2 1)))) '())


;; (f)
;; (solve-rectangle-puzzle lolon) Producing either the list of rectangles that
;;    describe a solution if one exists, or false if no solution can be found
;;    lolon are used to describe an initial puzzle.
;; solve-rectangle-puzzle: (listof (listof Nat)) -> (anyof (listof Rect) false)
;; Example:
(check-expect (solve-rectangle-puzzle puzz)
              (list
               (make-rect 5 5 2 2)
               (make-rect 4 5 1 2)
               (make-rect 1 5 3 2)
               (make-rect 0 5 1 2)
               (make-rect 0 4 4 1)
               (make-rect 5 3 2 2)
               (make-rect 5 2 2 1)
               (make-rect 4 2 1 3)
               (make-rect 4 1 2 1)
               (make-rect 2 1 2 3)
               (make-rect 1 1 1 3)
               (make-rect 6 0 1 2)
               (make-rect 1 0 5 1)
               (make-rect 0 0 1 4)))


(define (solve-rectangle-puzzle lolon)
  (local
    [(define result (search solved? neighbours (construct-puzzle lolon)))]
    (cond
      [(boolean? result) result]
      [else
       (state-rects result)])))
  
;; Tests:
(check-expect (solve-rectangle-puzzle (list (list 2 0) (list 0 1))) false)
(check-expect (solve-rectangle-puzzle
               (list (list 1 2 0) (list 0 3 0) (list 4 0 1))) false)
(check-expect (solve-rectangle-puzzle
               (list (list 1 2 3 4 5)
                     (list 0 0 0 0 0)
                     (list 0 0 0 0 0)
                     (list 0 0 0 0 0)
                     (list 4 3 2 1 0)))
              (list
               (make-rect 3 4 1 1)
               (make-rect 2 3 1 2)
               (make-rect 1 2 1 3)
               (make-rect 0 1 1 4)
               (make-rect 4 0 1 5)
               (make-rect 3 0 1 4)
               (make-rect 2 0 1 3)
               (make-rect 1 0 1 2)
               (make-rect 0 0 1 1)))
(check-expect (solve-rectangle-puzzle big-puzz)
              (list
               (make-rect 11 12 2 1)
               (make-rect 3 12 8 1)
               (make-rect 0 12 3 1)
               (make-rect 9 11 4 1)
               (make-rect 5 11 4 1)
               (make-rect 5 8 8 3)
               (make-rect 4 8 1 4)
               (make-rect 7 7 6 1)
               (make-rect 4 7 2 1)
               (make-rect 0 7 4 5)
               (make-rect 6 6 1 2)
               (make-rect 1 6 5 1)
               (make-rect 7 5 2 2)
               (make-rect 1 4 1 2)
               (make-rect 0 4 1 3)
               (make-rect 7 3 2 2)
               (make-rect 2 3 1 3)
               (make-rect 12 2 1 5)
               (make-rect 7 1 1 2)
               (make-rect 3 1 4 5)
               (make-rect 2 1 1 2)
               (make-rect 1 1 1 3)
               (make-rect 12 0 1 2)
               (make-rect 9 0 3 7)
               (make-rect 8 0 1 3)
               (make-rect 1 0 7 1)
               (make-rect 0 0 1 4)))
              


              