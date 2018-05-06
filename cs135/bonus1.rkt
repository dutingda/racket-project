;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname bonus1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 01, Problem 4(bonus)
;;***************************************************
;;

;;Useful constants
(define first-midterm-weight 0.1)
(define second-midterm-weight 0.2)
(define final-exam-weight 0.45)
(define assignments-weight 0.2)
(define participation-mark 100)
(define participation-weight 0.05)

;; (final-cs135-grade first-midterm second-midterm final-exam assignments) produces
;;   final grade of the cs135 course
;; (final-cs135-grade Nat Nat Nat -> Num)
;; Requires: first-midterm, second-midterm, final-exam, assignments<=100
;; Examples:
(check-expect(final-cs135-grade 30 20 60 47) 46)

(define(final-cs135-grade first-midterm second-midterm final-exam assignments)
  (min (+ (* first-midterm first-midterm-weight)
          (* second-midterm second-midterm-weight)
          (* final-exam final-exam-weight)
          (* assignments assignments-weight)
          (* participation-mark participation-weight))
       (expt 46 (+ 2 (min (sgn (- (+ (* final-exam final-exam-weight)
                                     (* second-midterm second-midterm-weight)
                                     (* first-midterm first-midterm-weight)) 50)) (sgn (- assignments 50)))))))

;; Tests:
(check-expect(final-cs135-grade 60 70 100 85) 87)
(check-expect(final-cs135-grade 60 90 85 50) 77.25)
(check-expect(final-cs135-grade 60 90 50 50) 46)
(check-expect(final-cs135-grade 80 70 49 50) 46)
(check-expect(final-cs135-grade 29 29 20 100) 42.7)
(check-expect(final-cs135-grade 70 40 60 100) 46)
(check-expect(final-cs135-grade 100 100 100 49) 46)
