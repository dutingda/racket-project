;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname moretabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 07, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Useful constants:
(define (add3 x) (+ x 3))
(define t1 '((3425 3 13 3)
             (1 2 3 5)
             (-1.2 0.44 3.1 11)
             (-12 -3 -4.4 5.3)
             (1 2 4 7)))
(define t2 '((-2 2.1 -3 3.1)
             (3 2.1 0 2.1)))
(define t3 '((7 11.2 13.4 99)
             (8 7.3 3.2 11)
             (6 11 11 0)
             (8.74 12.22 -100.01 -1.02)))
(define t4 (append t1 t2 t3))
(define t5 (append t1 t3))
(define t6 (append t2 t3))
(define t7 (append t1 t2))

(define t8 '((3425 3 13 3 1 2 3 5)
             (-1.2 0.44 3.1 11 -12 -3 -4.4 5.3)
             (1 2 4 7 -2 2.1 -3 3.1)
             (3 2.1 0 2.1 7 11.2 13.4 99)
             (8 7.3 3.2 11 8.74 12.22 -10.01 -1.02)))
 
  
;; (apply-funtion f arg) produces the result of f with the given argument arg
;; apply-function: (X -> Y) X -> Y
(define (apply-function f arg)
  (f arg))


;; (mirror table) Produces the table that was reversed the elements of each row from
;;   the original table
;; mirror: Table -> Table
;; Examples:
(check-expect (mirror '((-3.2 4.5 7) (13 3 -3)))
              (list (list 7 4.5 -3.2)
                    (list -3 3 13)))
(check-expect (mirror empty) empty)


(define (mirror table)
  (local
    [;; (mirror-help list acc) Produces a reversed list of "list" with the help of the
     ;;    accumulator acc
     ;; mirror-help: (listof Num) (listof Num) -> (listof Num)
     (define (mirror-help list acc)
       (cond
         [(empty? list) acc]
         [else
          (mirror-help
           (rest list)
           (cons (first list) acc))]))]
    (cond
      [(empty? table) empty]
      [else
       (cons (mirror-help (first table) empty)
             (mirror (rest table)))])))


;; Tests:
(check-expect (mirror t1)
              (list (list 3 13 3 3425)
                    (list 5 3 2 1)
                    (list 11 3.1 0.44 -1.2)
                    (list 5.3 -4.4 -3 -12)
                    (list 7 4 2 1)))
(check-expect (mirror '((1))) (list (list 1)))
(check-expect (mirror t2)
              (list
               (list 3.1 -3 2.1 -2)
               (list 2.1 0 2.1 3)))
(check-expect (mirror t3)
              (list
               (list 99 13.4 11.2 7)
               (list 11 3.2 7.3 8)
               (list 0 11 11 6)
               (list -1.02 -100.01 12.22 8.74)))

;; (b)
;; (element-apply-many lof table) Produces a list of tables that was repectively
;;    applied with the position that was in the list of function lof
;; element-apply-many: (listof (AnyOf (Num -> Num) (Num -> Int) (Num -> Nat))) Table
;;                      -> (listof Table)
;; Examples:
(check-expect (element-apply-many (list abs floor add3) '((7 4.5 -3.2) (-3 3 13)))
              (list (list (list 7 4.5 3.2)
                          (list 3 3 13))
                    (list (list 7 4 -4)
                          (list -3 3 13))
                    (list (list 10 7.5 -0.2)
                          (list 0 6 16))))
(check-expect (element-apply-many (list abs floor add3) empty)
              (list empty empty empty))
(check-expect (element-apply-many empty t8) empty)
(check-expect (element-apply-many empty empty) empty)


(define (element-apply-many lof table)
  (local
    [;; (onefntorow f row) produces a list which was applied the function f to the row
     ;; onefntorow: (AnyOf (Num -> Num) (Num -> Int) (Num -> Nat))
     ;;             (listof Num) -> (listof Num)
     (define (onefntorow f row)
       (cond
         [(empty? row) empty]
         [else
          (cons (f (first row)) (onefntorow f (rest row)))]))
     ;; (combine loa f) Produces the Table that is applied by the function with the step
     ;;    of applying the function f in each row
     ;; combine: Table (AnyOf (Num -> Num) (Num -> Int) (Num -> Nat)) -> Table
     (define (combine loa f)
       (cond
         [(empty? loa) empty]
         [else
          (cons (onefntorow f (first loa)) (combine (rest loa) f))]))]
    (cond
      [(empty? lof) empty]
      [else
       (cons (combine table (first lof))
             (element-apply-many (rest lof) table))])))
;; Tests:
(check-expect (element-apply-many (list add1 ceiling abs) t2)
              (list
               (list (list -1 3.1 -2 4.1) (list 4 3.1 1 3.1))
               (list (list -2 3 -3 4) (list 3 3 0 3))
               (list (list 2 2.1 3 3.1) (list 3 2.1 0 2.1))))


;; (c)
;; (define scale-smallest table offset) Produces a second function that consumes a
;;   number, multiplies that number by the smallest elemnt of the "table", and add the
;;   "offset"
;; scale-smallest: Table Num -> (Num -> Num)
;; require:
;;    table is a non-empty list
;; Examples:
(check-expect (apply-function (scale-smallest '((7 4.5 3.2) (-3 3 13)) 2.4) 7) -18.6)
(check-expect (apply-function (scale-smallest '((7 4.5 3.2) (-3 3 13)) 2.4) -2.7) 10.5)

(define (scale-smallest table offset)
  (local
    [;; (find-smallest row acc) Produces the minimum value of the row with the acc to
     ;;    help record the current minimum value
     ;;  find-smallest: (listof Num) Num -> Num
     (define (find-smallest row acc)
       (cond
         [(empty? row) acc]
         [(< (first row) acc)
          (find-smallest (rest row) (first row))]
         [else
          (find-smallest (rest row) acc)]))
     ;; (table-smallest-set table1) Produces a list of minimum value of each row in the
     ;;    Table "table1"
     ;; table-smallest-set: Table -> (listof Num)
     (define (table-smallest-set table1)
       (cond
         [(empty? (rest table1))
          (list (find-smallest (first table1) (first (first table1))))]
         [else
          (cons (find-smallest (first table1) (first (first table1)))
                (table-smallest-set (rest table1)))]))
     ;; (final num) Produces a second function that consumes a number, multiplies that
     ;;   number by the smallest element of the table and adds the offset
     ;; final: Num -> (Num -> Num)
     (define (final num)
       (+ (* num (find-smallest (table-smallest-set table) (first (first table))))
          offset))]
    final))
          
;; Tests:
(check-expect (apply-function (scale-smallest t4 1) 4) -399.04)
(check-expect (apply-function (scale-smallest t8 12) 7) -72)
(check-expect (apply-function (scale-smallest t7 3.1) 2) -20.9)