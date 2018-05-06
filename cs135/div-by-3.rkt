;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname div-by-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 04, Problem 3
;;***************************************************
;;


;; (a)
;; An natural number Nat3 is one of:
;; * 0
;; * 1
;; * 2
;; * (+ 3 Nat3)


;; (b)
;; (my-nat3: Nat3 -> Any)
;; (define (my-nat3-fn n)
;;  (cond
;;   [(zero? n) ...]
;;   [(= 1 n) ...]
;;   [(= 2 n) ...]
;;   [else
;;    (... (my-nat3-fn (- n 3)) ...)]))


;; (c)
;; (div-by-3? nat3-one) produces true if nat3-one is divisible by 3 and false otherwise
;; (div-by-3?: Nat3 -> Bool)
;; Example:
(check-expect (div-by-3? 27) true)
(check-expect (div-by-3? 0) true)
(check-expect (div-by-3? 1) false)
(check-expect (div-by-3? 2) false)


(define (div-by-3? nat3-one)
  (cond
    [(zero? nat3-one) true]
    [(= 1 nat3-one) false]
    [(= 2 nat3-one) false]
    [else
     (div-by-3? (- nat3-one 3))]))


;; Tests:
(check-expect (div-by-3? 4) false)
(check-expect (div-by-3? 6843299) false)
(check-expect (div-by-3? 29) false)


     