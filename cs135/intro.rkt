;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname intro) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 07, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (a)
;; (keep-ints loa) Produces a list that contains only the integers in the given
;;   list, in their original order
;; keep-ints: (listof Any) -> (listof Int)
;; Examples:
(check-expect (keep-ints (list 'a 1 'b 2)) (list 1 2))


(define (keep-ints loa)
  (filter integer? loa))


;; Tests:
(check-expect (keep-ints (list "abc" 2 3 "tingda" "dtd")) (list 2 3))
(check-expect (keep-ints (list 1 2 3 'a 'b (make-posn 6 7) 6))
              (list 1 2 3 6))

;; (b)
;; (contains? element my-list) produces true if
;;    the element is a member of my-list
;;    and false otherwise
;; containes?: Any (listof Any) -> Bool
;; Examples:
(check-expect (contains? 'fun (list 'racket 'is 'fun)) true)
(check-expect (contains? 'abc empty) false)



(define (contains? elem loa)
 (not (empty? (filter (lambda (x) (equal? elem x)) loa))))

;; Tests:
(check-expect (contains? 'bcd (list 'abc 'cde)) false)
(check-expect (contains? 'p (list "p" empty)) false)
(check-expect (contains? '1 (list '1 1)) true)


;; (c)
;; (lookup-al k alst) Produces the value corresponding to key k
;;   in alst; or false if k not present
;; lookup-al Num AL -> (Anyof Str Bool)
;; Examples:
(check-expect (lookup-al 'abc (list (list 'abc 1))) 1)
(check-expect (lookup-al 3 empty) false)


(define (lookup-al k alst)
  (local[;;final produces a AL which key is equal to k
         ;;final: (X -> Bool) Y -> Y
         (define final
         (filter (lambda (x) (equal? (first x) k)) alst))]
    (cond
      [(empty? final) false]
      [else (second (first final))])))


;; Tests:
(check-expect (lookup-al 102
                         (list (list 1 "dfsdk")
                               (list 2 "bca")
                               (list 3 "true"))) false)
(check-expect (lookup-al 0.1 (list (list 1 "dckdfjkds")
                                   (list 2 "")
                                   (list 3 "jfklsd"))) false)
(check-expect (lookup-al 2 (list (list 202 "ccc")
                                 (list 2 "facdk")
                                 (list 11 "three"))) "facdk")
(check-expect (lookup-al 1 (list (list 232 "kfjl")
                                 (list 13 "2")
                                 (list 1 "make-posn"))) "make-posn")


;; (d)
;; (extract-keys alst) Produces a list of all the keys in alst consumed in the same
;;   order they appear in the alst
;; extract-keys: AL -> (listof Num)               
;; Examples:
(check-expect (extract-keys (list (list 'defgh 1)(list 2 'mnopq))) (list 'defgh 2))
(check-expect (extract-keys empty) empty)

(define (extract-keys alst)
  (map first alst))


;; Tests:
(check-expect (extract-keys (list (list 3 'fdjks)
                                  (list 4 'skddri)
                                  (list 100 "fjkdsl"))) (list 3 4 100))
(check-expect (extract-keys (list (list 2 "kkk")
                                  (list 0 (make-posn 3 "abc")))) (list 2 0))
(check-expect (extract-keys (list (list 136 (list empty (list empty empty))))) (list 136))


;; (e)
;; (sum-positive lon) Pruduces the sum of positive numbers in the given list lon,
;;   the negative numbers will not be added
;; (listof Num) -> Num
;; Examples:
(check-expect (sum-positive (list 1 2 3 4 5)) 15)
(check-expect (sum-positive (list -11 -11111 -11 -1)) 0)

(define (sum-positive list-num)
  (foldr (lambda(x y)
           (cond [(> x 0) (+ x y)]
                 [else y])) 0 list-num))


;; Tests:
(check-expect (sum-positive '(-2 1 3 -4.3 12 3 0)) 19)
(check-expect (sum-positive '(2.75 0.25 -1 3 -3 1.2)) 7.2)


;; (f)
;; (countup-to n b) Produces a list from n to b
;;   countup-to: Int Int -> (listof Int)
;; requires: b >= n
;; Example:
(check-expect (countup-to 6 8) (list 6 7 8))

(define (countup-to n b)
  (build-list (+ (- b n) 1) (lambda (x) (+ x n))))

;; Tests:
(check-expect (countup-to 0 1) (list 0 1))
(check-expect (countup-to 1 4) (list 1 2 3 4))
(check-expect (countup-to 3 6) (list 3 4 5 6))


;; (g)
;; (shout los) Produces the list of strings from the list consumed but all strigns are
;;   in uppercase
;; shout: (listof Str) -> (listof Str)
;; Examples:
(check-expect (shout empty) empty)
(check-expect (shout '("get" "off" "my" "lawn")) '("GET" "OFF" "MY" "LAWN"))


(define (shout los)
  (map list->string
       (map (lambda (x) (map char-upcase x))(map string->list los))))


;; Tests:
(check-expect (shout (list "tingda")) '("TINGDA"))
(check-expect (shout (list "abcdefg" "gfedcba"))
              (list "ABCDEFG" "GFEDCBA"))
(check-expect (shout (list "abc" "is" "bcareversed"))
              (list "ABC" "IS" "BCAREVERSED"))


;; (h)
;; (make-validator loa) produces a predicate function and the produced function
;;   consumes an item and produces true if the item appears in the list consumed
;;   by make-validator and produces false otherwise
;; make-validator: (listof Any) -> (Any -> Bool)
;; Examples:
(check-expect ((make-validator empty) 'ABC) false)
(check-expect ((make-validator '(a b c)) 'b) true)


(define (make-validator list)
  (lambda (x) (contains? x list)))

(define primary-colour? (make-validator '(red blue green)))


;; Tests:
(check-expect (primary-colour? 'red) true)
(check-expect ((make-validator '(big small medium)) 'medium) true)
(check-expect ((make-validator empty) "dfdsf") false)
(check-expect ((make-validator '(0 1 0 10 2)) 2) true)
(check-expect ((make-validator (list "ddd" "cmd")) "cmd") true)
(check-expect ((make-validator (list (make-posn 'a 'b) (make-posn 'a 'b) 'd)) 'e) false)

