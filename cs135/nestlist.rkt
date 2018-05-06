;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nestlist) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 08, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Useful Functions:
(define (count-items nln) (nfoldr (lambda (x y) (add1 y)) + 0 nln))
(define (flatten lst) (nfoldr cons append empty lst))

;; (a)
;; (nfoldr f g b list) f is applied when the first element in the list
;;   is of type X, and g is applied when the first elementis a list. b
;;   is the base case and list is the nested list. This is a abstract function
;; nfoldr: (X Y -> Y) (Y Y -> Y) Y Nested-Listof-X -> Y
;; Examples:
(check-expect (count-items empty) 0)
(check-expect (flatten empty) empty)
(check-expect (count-items '(1 () ((4)))) 2)


(define (nfoldr f g b list)
  (cond
    [(empty? list) b]
    [(list? (first list))
     (g (nfoldr f g b (first list)) (nfoldr f g b (rest list)))]
    [else (f (first list)
             (nfoldr f g b (rest list)))]))


;; Tests:
(check-expect (count-items '(1 (2 3) () ((4)))) 4)
(check-expect (flatten '(1 (2 3) () ((4)))) '(1 2 3 4))


;; (b)
;; (nfilter f list) Produces the Nested-Listof-X that has been filtered by the
;;   function f
;; nfilter: (X -> Bool) Nested-Listof-X -> Nested-Listof-X
;; Example:
(check-expect (nfilter odd? '(1 (2 3) () ((4)))) '(1 (3) () (())))


(define (nfilter f list)
  (nfoldr (lambda (frst rror) (cond [(f frst) (cons frst rror)]
                                    [else rror])) cons empty list))


;; Tests:
(check-expect (nfilter even? '(1 (2 3) () ((4)))) '((2) () ((4))))
(check-expect (nfilter number? '(1 (2 3) () ((4)))) '(1 (2 3) () ((4))))
(check-expect (nfilter cons? '((() (())) (() (() (())) ()) () () ()))
              '((() (())) (() (() (())) ()) () () ()))
               
;; (c)
;; (nmap f list) Produces the Nested-Listof-X that is being operated by f, which is
;;   mapping, to every element in the list
;; nmap: (X -> Y) Nested-Listof-X -> Nested-Listof-Y
;; Example:
(check-expect (nmap sqr '(1 (2 3) () ((4)))) '(1 (4 9) () ((16))))

(define (nmap f list)
  (nfoldr (lambda (x y) (cons (f x) y)) cons empty list))


;; Tests:
(check-expect (nmap (lambda (x) (expt x 3)) '(1 (2 3) () ((4))))
              '(1 (8 27) () ((64))))
(check-expect (nmap add1 '(1 (2 3) () ((4))))
              '(2 (3 4) () ((5))))
(check-expect (nmap sub1 '(1 (2 3) () ((4))))
              '(0 (1 2) () ((3))))
                    

;; (d)
;; (nreverse list1) Produces the reversed list, also every nested list is also being
;;   reversed
;; nreverse: Nested-Listof-X -> Nested-Listof-X
;; Examples: 
(check-expect (nreverse '(1 (2 3) () ((4)))) '(((4)) () (3 2) 1))
(check-expect (nreverse '((1 (2 3)) 4 (5 (6 7 8) 9))) '((9 (8 7 6) 5) 4 ((3 2) 1)))


(define (nreverse list1)
  (nfoldr (lambda (x y) (append y (list x))) (lambda (lst lst2)
                                               (append lst2 (list lst)))
          empty list1))


;; Tests:
(check-expect (nreverse empty) empty)
(check-expect (nreverse '(1 3 (4 5) (6 7 8) 9)) '(9 (8 7 6) (5 4) 3 1))


;; (e)
;; (nheight list2) Produces the height of the Nested-Listof-X
;; nheight: Nested-Listof-X -> Nat
;; Example:
(check-expect (nheight '(1 2 3)) 1)
(check-expect (nheight '(1 (2 ()) 3)) 3)
(check-expect (nheight '(1 ())) 2) 

(define (nheight list2)
  (nfoldr (lambda (a c) c) (lambda (fi re) (max (add1 fi) re)) 1 list2))


;; Tests:
(check-expect (nheight '()) 1)
(check-expect (nheight '((1 (2 3)) 4 (5 (6 7 8) 9))) 3)
(check-expect (nheight '(a b c)) 1)
(check-expect (nheight '((1 a) (2 b) (3 c))) 2)
(check-expect (nheight '(1 (2 3) () ((4)))) 3)
  
;; (f)
;; (prune list2) moves all empty lists and any nested lists that only contain
;;   empty lists (i.e., they do not contain any elements of type X). Note that
;;   if there are no elements of type X, prune produces empty
;; prune: Nested-Listof-X -> Nested-Listof-X
;; Example:
(check-expect (prune '(() (3 ()) (() (() 4)))) '((3) ((4))))


(define (prune list3)
  (nfoldr cons (lambda (x y) (cond [(not (empty? x)) (cons x y)]
                                   [else y]))
          empty list3))


;; Tests:
(check-expect (prune '(1 (2 3 ()) ((()) (4) () (())))) '(1 (2 3) ((4))))
(check-expect (prune '(() ((()) ()))) '())
