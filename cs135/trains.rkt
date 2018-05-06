;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname trains) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 04, Problem 4
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a04lib.rkt")

;; The unit structure is defined in a04lib.rkt.  The require
;; statement, above, is all that's needed to have it take
;; effect here.  The following comment is here just so the
;; type definitions that follow make sense.

;; (define-struct unit (type serial))

;; -------- Q4a --------------
;; A Unit-Type is one of
;; * 'L
;; * 'B
;; * 'T
;; * 'P
;; * 'C

;; A Unit is a (make-unit Unit-Type Num)

;; A Train is one of:
;; * empty
;; * (cons Unit Train)


;; -------- Q4b --------------

;; string->train works by three diffferent parts. First, the helper function "char->unit"
;; helps to transfer from character to symbol in order to match the format of Unit. This
;; function is useful because the string is list of characters, thus there must be a step
;; which can be used to transfer from character to symbol. Secondly, there is "loc->train"
;; function, which uses the recurisive way to construct the train respectively in terms
;; of Unit and serials in the same index, which transfer the two list into one data type
;; This part is trying to use simply a list of characters and natural numbers to construct
;; the data type of Train. The last helper function, which is the last step, is to use
;; the built-in function "string->list" to change the string, which is what we consumes, and
;; corresponds the input of the "list" of characters, and combine everything together we
;; have constructed before to accomplish the goal.


;; -------- Q4c --------------

;; (headed-by? train1 type1) produces true if the first unit of the train1 has the given
;;   has the type1 and false otherwise
;; (head-by?: Train Unit-Type -> Bool)
;; Example:
(check-expect(headed-by? test-train 'L) true)


(define (headed-by? train1 type1)
  (cond
    [(empty? train1) false]
    [(symbol=? (unit-type (first train1)) type1) true]
    [else false]))


;; Tests:
(check-expect(headed-by? empty 'P) false)
(check-expect(headed-by? test-train 'B) false)
(check-expect(headed-by? test-train 'C) false)
(check-expect(headed-by? test-train 'T) false)
(check-expect(headed-by? test-train 'P) false)


;; -------- Q4d --------------

;; (ends-with-caboose? train2) produces true if and only if there is exactly one caboose
;;   and it is the last unit in the train
;; (ends-with-caboose?: Train -> Bool)
;; Example:
(check-expect(ends-with-caboose? test-train) true)
(check-expect (ends-with-caboose? empty) false)

(define (ends-with-caboose? train2)
  (cond
    [(empty? train2) false]
    [(empty? (rest train2)) 
     (symbol=? 'C (unit-type (first train2)))]
    [else
     (and (not (symbol=? 'C (unit-type (first train2))))
          (ends-with-caboose? (rest train2)))]))

;; Tests:
(check-expect (ends-with-caboose? (cons (make-unit 'C 7) empty)) true)
(check-expect (ends-with-caboose? (cons (make-unit 'L 8)
                                       (cons (make-unit 'T 3)
                                             (cons (make-unit 'P 9) empty)))) false)
(check-expect (ends-with-caboose? (cons (make-unit 'T 9)
                                       (cons (make-unit 'P 2)
                                             (cons (make-unit 'C 11)
                                                   (cons (make-unit 'L 5)
                                                         (cons (make-unit 'B 30)
                                                               (cons (make-unit 'C 10) empty))))))) false)


;; -------- Q4e --------------

;; (remove-unit t s) produces a Train that is identical to t except that the unit
;;    with the serial number s is removed
;; (remove-unit: Train Nat -> Train)
;; Example:
(check-expect (remove-unit test-train 3) (cons (make-unit 'L 2)
                                               (cons (make-unit 'C 5) empty)))
(check-expect (remove-unit empty 20) empty)


(define (remove-unit t s)
  (cond
    [(empty? t) empty]
    [(= s (unit-serial (first t))) (rest t)]
    [else
     (cons (first t) (remove-unit (rest t) s))]))


;; Tests:
(check-expect (remove-unit (cons (make-unit 'L 8)
                                 (cons (make-unit 'B 6)
                                       (cons (make-unit 'B 3)
                                             (cons (make-unit 'T 2)
                                                   (cons (make-unit 'P 1) empty))))) 3)
              (cons (make-unit 'L 8)
                    (cons (make-unit 'B 6)
                          (cons (make-unit 'T 2)
                                (cons (make-unit 'P 1) empty)))))
(check-expect (remove-unit (cons (make-unit 'C 7)
                                 (cons (make-unit 'P 13) empty)) 11)
              (cons (make-unit 'C 7)
                    (cons (make-unit 'P 13) empty)))


;; -------- Q4f --------------

;; (transfer type) produces 0, 1, 2 ,which represents locomotive, cars and
;;   caboose respectively for each "type" of the unit in the train
;; (transfer: Unit-Type -> (Anyof 0, 1, 2)
;; Example:
(check-expect (transfer (unit-type (make-unit 'P 14))) 1)


(define (transfer type)
  (cond
    [(symbol=? type 'L) 0]
    [(symbol=? type 'C) 2]
    [else 1]))




;; (proper-train? train) produces true if "train" is a proper train and false otherwise
;;   A proper train is a Train with zero or more loocomotives followed by zero or more
;;   cars followed by zer or more cabooses and nothing after the last caboose.
;; (proper-train?: Train -> Bool)
;; Example:
(check-expect (proper-train? test-train) true)
(check-expect (proper-train? empty) true)
(check-expect (proper-train? (cons (make-unit 'C 19) empty)) true)


(define (proper-train? train)
  (cond
    [(empty? train) true]
    [(empty? (rest train)) true]
    [else
     (and (<= (transfer (unit-type (first train)))
              (transfer (unit-type (first (rest train)))))
          (proper-train? (rest train)))]))


;; Tests:
(check-expect (proper-train? (cons (make-unit 'L 8)
                                   (cons (make-unit 'B 6)
                                       (cons (make-unit 'B 3)
                                             (cons (make-unit 'T 2)
                                                   (cons (make-unit 'P 1) empty)))))) true)
(check-expect (proper-train? (cons (make-unit 'L 6)
                                   (cons (make-unit 'C 9)
                                         (cons (make-unit 'P 8) empty)))) false)

              





              





     

