;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname airmiles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 02, Problem 2
;;***************************************************
;;

;; Useful constants

(define standard-sponsored 15)
(define standard-non-sponsored 20)
(define premium-sponsored 10)
(define premium-non-sponsored 15)

;; (calc-airmiles amount-purchased card-type whether-sponsored) produces the number of
;;   Airmiles earned for given purchase
;; (calc-airmiles Num Sym Bool -> Nat)
;; Examples:
(check-expect(calc-airmiles 10.72 'premium true) 1)

(define (calc-airmiles amount-purchased card-type whether-sponsored)
  (cond
    [(and (symbol=? card-type 'standard) whether-sponsored) (floor (/ amount-purchased standard-sponsored))]
    [(and (symbol=? card-type 'standard) (not whether-sponsored)) (floor (/ amount-purchased standard-non-sponsored))]
    [(and (symbol=? card-type 'premium) whether-sponsored) (floor (/ amount-purchased premium-sponsored))]
    [(and (symbol=? card-type 'premium) (not whether-sponsored)) (floor (/ amount-purchased premium-non-sponsored))]))

;; Tests:
(check-expect(calc-airmiles 17.5 'standard false) 0)
(check-expect(calc-airmiles 78.5 'standard true) 5)
(check-expect(calc-airmiles 63 'premium true) 6)
(check-expect(calc-airmiles 60 'premium false) 4)
                                                          