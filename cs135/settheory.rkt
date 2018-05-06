;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname settheory) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 05, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A NumSet is a (listof Num)
;; requires: the numbers are strictly increasing

;; (a)
;; (union numsetone numsettwo) produces a single NumSet containing all the numbers that
;;   are in either of the numsetone or numsettwo
;; (union: NumSet NumSet -> NumSet)
;; Examples:
(check-expect(union '(1 2 3 6) '(1 3 4)) '(1 2 3 4 6))
(check-expect(union empty empty) empty)
(check-expect(union empty '(2 3 4)) '(2 3 4))
(check-expect(union '(1 4 38) empty) '(1 4 38))

(define (union numsetone numsettwo)
  (cond
    [(empty? numsetone) numsettwo]
    [(empty? numsettwo) numsetone]
    [(= (first numsetone) (first numsettwo))
     (cons (first numsetone) (union (rest numsetone) (rest numsettwo)))]
    [(< (first numsetone) (first numsettwo))
     (cons (first numsetone) (union (rest numsetone) numsettwo))]
    [else
     (cons (first numsettwo) (union numsetone (rest numsettwo)))]))


;; Tests:
(check-expect(union '(1 3 4 5 9) '(1 3 4 9 10)) '(1 3 4 5 9 10))
(check-expect(union '(2 7 29) '(3 19 29 34)) '(2 3 7 19 29 34))


;; (b)
;; (intersection lon1 lon2) produces a single NumSet contianing all the numbers that are
;;    in both of the lon1 and lon2
;; (intersection NumSet NumSet -> NumSet)
;; Examples:
(check-expect (intersection '(1 2 3 6) '(1 3 4)) '(1 3))
(check-expect (intersection empty '(1 2 3)) empty)
(check-expect (intersection '(2 3 4) empty) empty)

(define (intersection lon1 lon2)
  (cond
    [(or (empty? lon1) (empty? lon2)) empty]
    [(= (first lon1) (first lon2))
     (cons (first lon1) (intersection (rest lon1) (rest lon2)))]
    [(< (first lon1) (first lon2))
     (intersection (rest lon1) lon2)]
    [else
     (intersection lon1 (rest lon2))]))

;; Tests:
(check-expect (intersection '(1 3 5 7) '(3 7 9 11)) '(3 7))
(check-expect (intersection '(5 6 7 8) '(4 5 7 8 9)) '(5 7 8))

;; (c)
;; (difference lon1 lon2) produces a single NumSet containing all the numbers that are
;;   in the lon1 but not the lon2
;; Examples:
(check-expect (difference '(1 3 4 5) '(1 2 3)) '(4 5))
(check-expect (difference empty '(1 2 4 10)) empty)
(check-expect (difference '(1 2 3) empty) '(1 2 3))
              

(define (difference lon1 lon2)
  (cond
    [(empty? lon1) empty]
    [(empty? lon2) lon1]
    [(= (first lon1) (first lon2))
     (difference (rest lon1) (rest lon2))]
    [(< (first lon1) (first lon2))
     (cons (first lon1) (difference (rest lon1) lon2))]
    [else
     (difference lon1 (rest lon2))]))


;; Tests:
(check-expect (difference '(1 7 9 12 13) '(7 9 14 15)) '(1 12 13))
(check-expect (difference '(2 6 8 11) '(1 2 8 20)) '(6 11))


;; (d)
;; (symmetric-difference lon1 lon2) producees a single NumSet containing all numbers
;;    that are in the first set but not the second one
;; (symmetric-difference: NumSet NumSet -> NumSet)
;; Examples:
(check-expect (symmetric-difference '(1 2 3 5 6) '(1 3 4 5 9)) '(2 4 6 9))


(define (symmetric-difference lon1 lon2)
  (union (difference lon1 lon2)
         (difference lon2 lon1)))

;; Tests
(check-expect (symmetric-difference empty empty) empty)
(check-expect (symmetric-difference empty '(1 3 4 5)) '(1 3 4 5))
(check-expect (symmetric-difference '(1 2 3) empty) '(1 2 3))
(check-expect (symmetric-difference '(2 4 6) '(5 6 7 9)) '(2 4 5 7 9))
(check-expect (symmetric-difference '(4 5 7 8) '(2 4 5)) '(2 7 8))
     
  
      
   
     
     


