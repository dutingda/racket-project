
;; Ignore the next three lines
#lang racket
(require test-engine/racket-tests)
(provide string->train (struct-out unit) test-train last-digit other-digits)


(define-struct unit (type serial) #:transparent)
;; Data definition to be provided in trains.rkt.
;; ignore the #:transparent -- you'll learn about that in cs136


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEGIN SUPPORT FUNCTIONS: DO NOT MODIFY ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ------------ Q4 ----------------

(define test-train  (cons (make-unit 'L 2)
                          (cons (make-unit 'B 3)
                                (cons (make-unit 'C 5) empty))))

;; Exceeding 80 characters, but it's a nice, regular, easy to comprehend pattern.
(define default-serial-numbers
  (cons 2 (cons 3 (cons 5 (cons 7 (cons 11 (cons 13 (cons 17 (cons 19 (cons 23 (cons 29 empty)))))))))))

;; (char->unit char serial) creates a Unit
;;   from a char(acter) and a serial number
;; char->unit: (anyof #\L #\B #\T #\P #\C) Nat -> Unit
;; Example:
(check-expect (char->unit #\L 2) (make-unit 'L 2))

(define (char->unit char serial)
  (cond [(char=? char #\L) (make-unit 'L serial)]
        [(char=? char #\B) (make-unit 'B serial)]
        [(char=? char #\T) (make-unit 'T serial)]
        [(char=? char #\P) (make-unit 'P serial)]
        [(char=? char #\C) (make-unit 'C serial)]
        [else (error (format "Invalid Unit-Type: ~a." char))]))
(check-error (char->unit #\X 1) "Invalid Unit-Type: X.")


;; (loc->train loc los) constructs a Train
;;   from a list of characters (loc) and a list of serial numbers (los)
;; loc->train: (listof Char) (listof Nat) -> Train
;; Example:
(check-expect (loc->train (cons #\L (cons #\B (cons #\C empty)))
                          (cons 2 (cons 3 (cons 5 empty))))
              test-train)

(define (loc->train loc los)
  (cond [(empty? loc) empty]
        [(empty? los) (error "Ran out of serial numbers for train.")]
        [else (cons (char->unit (first loc) (first los))
                    (loc->train (rest loc) (rest los)))]))


;; (string->train units) builds a new train (with default serial numbers)
;;   from a string of unit types
;; string->train: Str -> Train
;; requires: characters of units corresponds to valid unit types
;;           units is less than or equal to 10 characters long
;; Example:
(check-expect (string->train "LBC") test-train)

(define (string->train units)
  (loc->train (string->list units) default-serial-numbers))


;; ------------ Bonus ----------------

;; A Digit is one of 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
;; A DecInt is one of:
;; * A Digit
;; * (+ (* b) d) where b is a DecInt and d is a Digit

;; (last-digit n) returns the last digit (base 10) of n
;; last-digit:  DecInt -> Digit
(check-expect (last-digit 123) 3)

(define (last-digit n)
  (remainder n 10))

;; Tests
(check-expect (last-digit 0) 0)
(check-expect (last-digit 1) 1)

;; (other-digits n) removes the last digit of n and returns the other digits
;; other-digits: DecInt -> DecInt 
;; Examples:
(check-expect (other-digits 12345) 1234)

(define (other-digits n)
  (quotient n 10))

;; Tests
(check-expect (other-digits 0) 0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END SUPPORT FUNCTIONS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

