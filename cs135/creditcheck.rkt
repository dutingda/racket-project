;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname creditcheck) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 03, Problem 2
;;***************************************************
;;

(define-struct date (year month day))
;; A Date is a (make-date Nat Nat Nat)
;; requires: year/month/day correspondds to a valid date
;;           (in the Gregorian calendar)

(define-struct transaction (tdate amount category))
;; A Transcaction is a (make-transaction Date Num Sym)

(define-struct account (name expires limit threshold exception))
;; An Account is a (make-account Str Date Num Num Sym)
;; requires: 0 < threshold < limit

;; (date<=? first-date second-date) produces true if the first Date occures before
;;   the second Date or they are the sam Date by comparing the first-date and the
;;   second-date
;; (date<=?: Date Date -> Bool)
;; Examples:
(check-expect (date<=? (make-date 2014 3 12) (make-date 2013 12 29)) false)


(define(date<=? first-date second-date)
  (cond
    [(< (date-year first-date) (date-year second-date)) true]
    [(> (date-year first-date) (date-year second-date)) false]
    [(< (date-month first-date) (date-month second-date)) true]
    [(> (date-month first-date) (date-month second-date)) false]
    [(< (date-day first-date) (date-day second-date)) true]
    [(> (date-day first-date) (date-day second-date)) false]
    [else true]))


;; Tests:
(check-expect(date<=? (make-date 2014 7 20) (make-date 2014 6 30)) false)
(check-expect(date<=? (make-date 2018 5 17) (make-date 2018 5 19)) true)
(check-expect(date<=? (make-date 2000 2 29) (make-date 2000 2 29)) true)


;; (approve? transaction account) produces true if the transaction amount of "transaction"
;;   does not exceed the account limit of the "account"and the transaction date of the "transaction"
;;   in not after the date the card expires of the "account", otherwise it produces false
;; (approve?: Transaction Account -> Bool)
;; Example:
(check-expect(approve? (make-transaction (make-date 2012 8 17) 300 'entertainment)
                       (make-account "Tingda Du" (make-date 2019 3 1) 250 100 'food)) false)


(define(approve? transaction account)
  (and (<= (transaction-amount transaction) (account-limit account))
       (date<=? (transaction-tdate transaction) (account-expires account))))


;; Tests:
(check-expect(approve? (make-transaction (make-date 2012 8 17) 200 'gas)
                       (make-account "Bob Smith" (make-date 2012 8 1) 250 100 'entertainment)) false)
(check-expect(approve? (make-transaction (make-date 2018 8 17) 100 'food)
                       (make-account "Tingda Du" (make-date 2018 8 19) 250 100 'food)) true)


;; (alert? trans acc) produces a true if the transaction("trans") is approved, but he transaction
;;    amount exceeds the threshold of the "acc" and the transaction category of "trans" is not
;;    the exception category of the "acc", othewise it will produce false.
;; (alert?: Transaction Account -> Bool)
;; Example:
(check-expect(alert? (make-transaction (make-date 2015 6 30) 170 'entertainment)
                     (make-account "Tingda Du" (make-date 2019 8 31) 500 150 'food)) true)


(define(alert? trans acc)
  (cond
    [(not (approve? trans acc)) false]
    [else (cond
            [(and (> (transaction-amount trans) (account-threshold acc))
                  (not (symbol=? (transaction-category trans) (account-exception acc)))) true]
            [else false])]))


;; Tests:
(check-expect(alert? (make-transaction (make-date 2015 6 30) 501 'entertainment)
                     (make-account "Tingda Du" (make-date 2015 6 30) 500 150 'food)) false)
(check-expect(alert? (make-transaction (make-date 2019 6 30) 170 'food)
                     (make-account "Bob Smith" (make-date 2019 8 31) 500 160 'food)) false)





