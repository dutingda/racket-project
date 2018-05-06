;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ca) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;
;; *************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 09, Problem 1
;; *************************************************
;;

;; (a)
;; (number a b c) Produces natural number between 0 and 7 that is represented
;;    by the binary number a b c
;; number: Nat Nat Nat ->  Nat
;; Examples:
(check-expect (number 1 1 1) 7)
(check-expect (number 0 0 1) 1)

(define (number f s t)
  (+ (* t (expt 2 0))
     (* s (expt 2 1))
     (* f (expt 2 2))))


;; (apply-rule a b c r) Produces either 0 or 1 depending on whether the resulting square
;;   should be white or black by applying the given CA rule and using 3 natural numbers
;;   a b c and one rule number r
;; apply-rule: Nat Nat Nat Nat -> (Anyof 0 1)
;; requires:
;;             0 <= r <= 255
;; Examples:
(check-expect (apply-rule 1 1 0 86) 1)
(check-expect (apply-rule 1 1 1 86) 0)




(define (apply-rule a b c r)
  (cond
    [(odd? (floor (/ r (expt 2 (number a b c))))) 1]
    [else 0]))


;; Tests:
(check-expect (apply-rule 1 0 1 345) 0)
(check-expect (apply-rule 0 0 1 327) 1)
(check-expect (apply-rule 0 1 1 279) 0)

;; (b)
;; (hp list1 r) Produce a new list of 0s and 1s, of the same length
;;   as the input list, containing the result of applying the given rule at
;;   at every position in the original list with rule number
;; hp: (listof 0 1) Nat -> (listof 0 1)
;; Examples:
(check-expect (hp '(0 1 1 1 0 0 1) 826) (list 1 0 0 1 1))
(check-expect (hp '(1) 101) '())
(check-expect (hp '(1 0 0 1 0) 1234) (list 1 1 0))



(define (hp list1 r)
  (cond
    [(< (length list1) 3) empty]
    [else
     (cons (apply-rule (first list1) (second list1) (third list1) r)
           (hp (rest list1) r))]))


;; (next-row lst r) Produce a new list of 0 and 1, of the same length
;;   as the input list, containing the result of applying the given rule at
;;   at every position in the original list with rule number
;; next-row: (listof 0 1) Nat -> (listof 0 1)
;; Examples:
(check-expect (next-row '(1 0 1) 86) (list 1 0 1))
(check-expect (next-row '(0 1 1 1 1 1) 36) (list 0 0 0 0 0 0))
(check-expect (next-row '(1 1 1 1 0 1) 22) (list 0 0 0 0 0 1))

(define (next-row lst r)
    (hp (append (list 0) lst (list 0)) r))

;;Test:
(check-expect (next-row '(0 1 1 0 1 0 1 1 1 0 0 0 1 0 1) 1256)
              (list 0 1 1 1 0 1 1 1 1 0 0 0 0 1 0))
(check-expect (next-row '(1 1 1 1 1 0) 43) (list 1 0 0 0 0 0))

;; (c)
;; (iterate f b n) Produces a list of length n containing (b, (f(b).
;;   (f(f(b),......,f(^n-1)(b)), where each f^i(b) is the result of applying
;;   f t times to b
;; iterate: function Nat Nat -> (listof Nat)
;; Examples:
(check-expect (iterate sub1 23 1) (list 23))
(check-expect (iterate floor 12 0) empty)
(check-expect (iterate sqr 10 4) (list 10 100 10000 100000000))


(define (iterate f b n)
  (cond
    [(= n 0) empty]
    [(= n 1) (list b)]
    [else (cons b (iterate f (f b) (sub1 n)))]))


;;Test:
(check-expect (iterate sqrt 4 2) (list 4 2))
(check-expect (iterate add1 4 3) (list 4 5 6))
(check-expect (iterate add1 2 1) '(2))
(check-expect (iterate add1 8 5) '(8 9 10 11 12))
(check-expect (iterate (lambda (x) (sub1 (sqr x))) 7 3)
              '(7 48 2303))
(check-expect (iterate sub1 0 8) '(0 -1 -2 -3 -4 -5 -6 -7))

;; (d)
;; (run-automaton list3 rn n) Produces a list of n lists, where the
;;   first sub-list is the consumed list, and each subsequent row is the
;;   resut of applying the CA rule to rhw row befor it by using rule number
;;   rn
;; run-automaton: (listof Nat) Nat Nat -> (listof (listof Nat))
;; Examples:
(check-expect (run-automaton (list 1 1 1) 70 2) (list (list 1 1 1) (list 0 0 1)))
(check-expect (run-automaton (list 1 1 0) 123 0) empty)

(define (run-automaton list3 rn n)
  (iterate (lambda (list3) (next-row list3 rn)) list3 n))


;;Test:
(check-expect (run-automaton '(1 1 1 0 1 1) 89 1)
              '((1 1 1 0 1 1)))
(check-expect (run-automaton empty 32 3)
              (list '() '() '()))
(check-expect (run-automaton '(1) 12 4)
              (list (list 1) (list 1) (list 1) (list 1)))
(check-expect (run-automaton '(1 1 0 1 0) 86 3)
              '((1 1 0 1 0) (0 1 0 1 1) (1 1 0 0 1)))
(check-expect (run-automaton '(1 1 0 1 0) 100 3)
              '((1 1 0 1 0) (0 1 1 1 0) (0 0 0 1 0)))
(check-expect (run-automaton '(0 1 0 1 1 1 1 0) 26 10)
              (list
               (list 0 1 0 1 1 1 1 0)
               (list 1 0 0 1 0 0 0 1)
               (list 0 1 1 0 1 0 1 0)
               (list 1 1 0 0 0 0 0 1)
               (list 1 0 1 0 0 0 1 0)
               (list 0 0 0 1 0 1 0 1)
               (list 0 0 1 0 0 0 0 0)
               (list 0 1 0 1 0 0 0 0)
               (list 1 0 0 0 1 0 0 0)
               (list 0 1 0 1 0 1 0 0)))
