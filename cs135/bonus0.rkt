;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 04, Problem 5(bonus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a04lib.rkt")

(define (digit-sum n)
  (cond
    [(< n 10) n]
    [else
     (+ (last-digit n)
        (digit-sum (other-digits n)))]))

(define (div-by-3? nat)
  (cond
    [(zero? nat) true]
    [(= 1 nat) false]
    [(= 2 nat) false]
    [else
     (div-by-3? (- nat 3))]))

(define (div-by-3-alt? natu)
  (div-by-3? (digit-sum natu)))

(check-expect (div-by-3-alt? 46533431) false)
(check-expect (div-by-3-alt? 1312312) false)
(check-expect (div-by-3-alt? 3482947329) true)

     