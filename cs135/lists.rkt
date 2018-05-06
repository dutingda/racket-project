;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 04, Problem 2
;;***************************************************
;;

;; (a)
;; (sum-positive integer-list) produces the sum of the positive integers in the
;;    integer-list
;; (sum-positive: (listof Int) -> Int)
;; Examples:
(check-expect (sum-positive (cons 5 (cons -3 (cons 4 empty)))) 9)
(check-expect (sum-positive empty) 0)

(define (sum-positive integer-list)
  (cond
    [(empty? integer-list) 0]
    [(positive? (first integer-list))
     (+ (first integer-list) (sum-positive (rest integer-list)))]
    [else
     (sum-positive (rest integer-list))]))


;; Tests:
(check-expect (sum-positive (cons 0 empty)) 0)
(check-expect (sum-positive (cons -9 (cons -100 (cons 4 empty)))) 4)
(check-expect (sum-positive (cons -10 (cons -12 (cons -1 empty)))) 0)


;; (b)
;; (contains? elem list) produces ture if elem is in the "list" and false otherwise
;; (contains?: Any (listof Any) -> Bool
;; Example:
(check-expect  (contains? 'fun (cons 'racket (cons 'fun (cons 'is empty)))) true)
(check-expect (contains? 'empty empty) false)


(define (contains? elem list)
  (cond
    [(empty? list) false]
    [else
     (or (contains? elem (rest list)) (equal? elem (first list)))]))


;; Tests
(check-expect (contains? 6
                         (cons 'fun (cons false (cons "6" (cons (make-posn 8 7) empty)))))
              false)
(check-expect (contains? false (cons false (cons "false" (cons 'false empty)))) true)
(check-expect (contains? "let" (cons 'let (cons "let" (cons true (cons false empty)))))
              true)


;; (c)
;; (has-duplicate? list) produces true if any element in the "list" appears more than
;;    once, otherwise it produces false
;; (has-duplicate?: (listof Any) -> Bool
;; Examples:
(check-expect (has-duplicate? (cons 1 (cons 2 (cons 2 empty)))) true)
(check-expect (has-duplicate? empty) false)

(define (has-duplicate? list)
  (cond
    [(empty? list) false]
    [else
     (or (has-duplicate? (rest list)) (contains? (first list) (rest list)))]))


;; Tests:
(check-expect (has-duplicate? (cons 2 (cons false (cons 'false (cons 1 empty))))) false)
(check-expect (has-duplicate? (cons (make-posn "a" 'cons) (cons (make-posn "a" 'cons)
                                                              (cons false empty)))) true)
(check-expect (has-duplicate? (cons 'no-duplicate empty)) false)


;; (d)
;; (keep-ints list) produces a list that contains only the integers in the given list, in
;;    the "list" appears more than once.
;; (keep-ints: (listof Any) -> (listof Any))
;; Example:
(check-expect (keep-ints (cons 'a (cons 1 (cons "b" (cons 2 empty))))) (cons 1 (cons 2 empty)))
(check-expect (keep-ints empty) empty)

(define (keep-ints list)
  (cond
    [(empty? list) empty]
    [(integer? (first list))
     (cons (first list) (keep-ints (rest list)))]
    [else
     (keep-ints (rest list))]))


;; Tests:
(check-expect (keep-ints (cons "djfk" (cons 6 (cons 'p (cons "6" (cons 'p empty))))))
              (cons 6 empty))
(check-expect (keep-ints (cons 'empty (cons 82 (cons 8.0 (cons 6.2 (cons 3/3 (cons "c" empty)))))))
                         (cons 82 (cons 8.0 (cons 1 empty))))
     

                                                              
     
     
     

