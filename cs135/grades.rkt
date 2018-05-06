;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname grades) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 02, Problem 3
;;***************************************************
;;

;;Useful constants
(define first-midterm-weight 0.1)
(define second-midterm-weight 0.2)
(define final-exam-weight 0.45)
(define assignments-weight 0.2)
(define participation-weight 0.05)

;; Helper function
;; (mark-calc first-midterm second-midterm final-exam assignments participation)
;;   produces a normal mark calculation of final grade with each component weighted
;; (mark-calc: Nat Nat Nat Nat Nat -> Num)
;; requires: first-midterm, second-midterm, final-exam, assignments, participation <= 100
;; Examples:
(check-expect(mark-calc 80 75 85 90 100) 84.25)

(define(mark-calc first-midterm second-midterm final-exam assignments participation)
  (+ (* first-midterm first-midterm-weight)
     (* second-midterm second-midterm-weight)
     (* final-exam final-exam-weight)
     (* assignments assignments-weight)
     (* participation participation-weight))) 

;; (final-cs135-grade first-midterm second-midterm final-exam assignments participation)
;;    produces the final grade in the course as a percentage
;; (final-cs135-grade: Nat Nat Nat Nat Nat-> Num)
;; requires: first-midterm, second-midterm, final-exam, assignments, participation <= 100
;; Examples:

(check-expect(final-cs135-grade 80 50 30 75 100) 46)

(define(final-cs135-grade first-midterm second-midterm final-exam assignments participation)
  (cond
    [(or (< (+ (* final-exam final-exam-weight)
               (* second-midterm second-midterm-weight)
               (* first-midterm first-midterm-weight)) 50)
         (< assignments 50))
     (cond
       [(>= (mark-calc first-midterm second-midterm final-exam assignments participation) 46) 46]
       [else (mark-calc first-midterm second-midterm final-exam assignments participation)])]
    [else (mark-calc first-midterm second-midterm final-exam assignments participation)]))

;; Test:
(check-expect(final-cs135-grade 29 29 20 100 100) 42.7)
(check-expect(final-cs135-grade 40 59 94 44 83) 46)
(check-expect(final-cs135-grade 0 0 0 0 0) 0)
(check-expect(final-cs135-grade 100 100 100 100 100) 100)
(check-expect(final-cs135-grade 60 84 92 50 48) 76.6)
(check-expect(final-cs135-grade 67 83 73 95 28) 76.55)
           

