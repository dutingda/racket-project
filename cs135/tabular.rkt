;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname tabular) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 05, Problem 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (a)
;; (times-row lon n) produces a list of number that has beem multiplied by the number
;;    the original list lon
;; (times-row: (listof Num) Num -> (listof Num))
;; 
(check-expect (times-row (list 8 3 4 9) 2)
              (list 16 6 8 18))
(check-expect (times-row empty 1) empty)

(define (times-row lon n)
  (cond
    [(empty? lon) empty]
    [else
     (cons (* n (first lon))
           (times-row (rest lon) n))]))

;; (mult-by n table) produces the table resulting forom each number in the original
;;    table being multiplied by the consumed number n
;; (mult-by: Num Table)
;; Example:
(check-expect (mult-by -1 (list (list 8 3 4 9) (list 3 7 5 6) (list -1 1 -3 0)))
              (list (list -8 -3 -4 -9) (list -3 -7 -5 -6) (list 1 -1 3 0)))
(check-expect (mult-by 3 empty) empty)

(define (mult-by n table)
  (cond
    [(empty? table) empty]
    [else
     (cons (times-row (first table) n)
           (mult-by n (rest table)))]))


;; Tests:
(check-expect (mult-by 3 (list (list 2 3 4 5) (list 1 2 3 4) (list 3 1 2 3)))
              (list (list 6 9 12 15) (list 3 6 9 12) (list 9 3 6 9)))
(check-expect (mult-by 1 (list (list 0 0 0) (list 1 3 2) (list 2 1 2)))
                       (list (list 0 0 0) (list 1 3 2) (list 2 1 2)))


;; (b)

;; (get-position n lon) produces the element in the nth position in the list, false if
;;   the index exceed the list
;; (get-position: Nat (listof Any) -> Any)
;; Example:
(check-expect (get-position 3 (list 2 4 1 3 9)) 3)
(check-expect (get-position 0 (list 4 1 1 2 0)) 4)
(check-expect (get-position 43 (list 3 1 2)) false)
(check-expect (get-position 2 (list 4 (make-posn 3 2) empty)) empty)

(define (get-position n lon)
  (cond
    [(empty? lon) false]
    [(= n 0) (first lon)]
    [else
     (get-position (sub1 n) (rest lon))]))

;; (get-elem row column table) produces the number which is in that row and column in
;;   the table. If the table is not large enough in either dimension this function
;;   should produce false
;; (get-elem: Nat Nat Table -> Num)
;; Examples:
(check-expect (get-elem 2 1 (list (list 8 3 4 9)
                                  (list 3 7 5 6)
                                  (list -1 1 -3 0))) 1)
(check-expect (get-elem 4 1 (list (list 8 3 4 9)
                                  (list 3 7 5 6)
                                  (list -1 1 -3 0))) false)

(define (get-elem row column table)
  (cond
    [(boolean? (get-position row table)) false]
    [else
     (get-position column (get-position row table))]))


;; Tests:
(check-expect (get-elem 1 3 (list (list 3 1 2 7 3)
                                  (list 1 2 7 3 4)
                                  (list 2 -1 3 2 0)
                                  (list 1 2 38 -7.3 12))) 3)

;; (c)
;; (define col column table) produces a list of all numbers in that column in the table,
;;   when read from top to bottom. If there is no such column, this function should produce
;;   empty list
;; (col: Nat Table -> (listof Num))
;; Examples:
(check-expect (col 2 (list (list 8 3 4 9)
                           (list 3 7 5 6)
                           (list -1 1 -3 0))) (list 4 5 -3))
(check-expect (col 1 empty) empty)
(check-expect (col 2 (list (list 1 2)
                           (list 3 4)
                           (list 4 3))) empty)

(define (col column table)
  (cond
    [(empty? table) empty]
    [(boolean? (get-position column (first table))) empty]
    [else
     (cons (get-position column (first table))
           (col column (rest table)))]))

;; Tests:
(check-expect (col 0 (list (list 4 5)
                           (list 2 4)
                           (list 1 0))) (list 4 2 1))

;; (d)

;; (sum-twolist lon1 lon2) produces the sum list of two element of two lists lon1 lon2
;; (sum-twolist: (listof Num) (listof Num) -> (listof Num))
;; Examples:
(check-expect (sum-twolist empty empty) empty)
(check-expect (sum-twolist (list 3 1 4) (list -1 -2 13))
              (list 2 -1 17))

(define (sum-twolist lon1 lon2)
  (cond
    [(empty? lon1) empty]
    [else
     (cons (+ (first lon1) (first lon2))
           (sum-twolist (rest lon1) (rest lon2)))]))
     

;; (sum-tables table1 table2) produces the table which results from adding up the
;;   which results from adding up the elements pairwise between the two tables, which
;;   is table1 and table2 (with same dimensions)
;; (sum-tables: Table Table -> Table)
;; Examples:
(check-expect (sum-tables (list (list 1 2)
                                (list 4 7))
                          (list (list 3 1)
                                (list 0 1))) (list (list 4 3)
                                                   (list 4 8)))
(check-expect (sum-tables empty empty) empty)


(define (sum-tables table1 table2)
  (cond
    [(empty? table1) empty]
    [else
     (cons (sum-twolist (first table1) (first table2))
           (sum-tables (rest table1) (rest table2)))]))
                          
;; Tests:
(check-expect (sum-tables (list (list 2))
                          (list (list 0)))
              (list (list 2)))
(check-expect (sum-tables (list (list 0 1 -2)
                                (list 7 3 -1))
                          (list (list 8 6 2)
                                (list -1 0 -1)))
              (list (list 8 7 0)
                    (list 6 3 -2)))
                   

    


  
              





