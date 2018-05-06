;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |grades |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 01, Problem 3
;;***************************************************
;;

;;Useful constants:
(define first-midterm-weight 0.1)
(define second-midterm-weight 0.2)
(define final-exam-weight 0.45)
(define assignments-weight 0.2)
(define participation-mark 100)
(define participation-weight 0.05)
(define minimum-grade-achieved 60)

;; (a)
;; (final-cs135-grade first-midterm second-midterm final-exam assignments) produces
;;   the final grade in the course as a percentage
;; (final-cs135-grade: Nat Nat Nat -> Num)
;;  requires: first-midterm, second-midterm, final-exam, assignments<=100
;; Examples:
(check-expect(final-cs135-grade 80 75 85 90) 84.25)

(define(final-cs135-grade first-midterm second-midterm final-exam assignments)
  (+ (* first-midterm first-midterm-weight)
     (* second-midterm second-midterm-weight)
     (* final-exam final-exam-weight)
     (* assignments assignments-weight)
     (* participation-mark participation-weight)))

;; Tests:
(check-expect(final-cs135-grade 30 20 50 80) 50.5)
(check-expect(final-cs135-grade 0 0 0 0) 5)
(check-expect(final-cs135-grade 100 100 100 20) 84)

;; (b)
;; (cs135-final-exam-grade-needed first-midterm second-midterm assignments) produces
;;   a percentage grade which is needed for final exam in order to obtain 60% in the
;;   course
;; (cs135-final-exam-grade-needed: Nat Nat Nat -> Num)
;; requires: first-midterm, second-midterm, assignments<=100
;; Examples:
(check-expect(cs135-final-exam-grade-needed 30 70 60) 520/9)

(define(cs135-final-exam-grade-needed first-midterm second-midterm assignments)
  (/ (- minimum-grade-achieved (+ (* first-midterm first-midterm-weight)
                                  (* second-midterm second-midterm-weight)
                                  (* assignments assignments-weight)
                                  (* 100 0.05))) final-exam-weight))

;;Tests:
(check-expect(cs135-final-exam-grade-needed 100 90 80) 220/9)
(check-expect(cs135-final-exam-grade-needed 10 20 15) 940/9)
(check-expect(cs135-final-exam-grade-needed 100 100 100) 100/9)