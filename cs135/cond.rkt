;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname cond) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 02, Problem 1
;;***************************************************
;;

;;(a)

(define(q1a x)
  (cond
    [(and (p2? x) (p1? x)) 'left]
    [(and (p2? x) (not (p1? x))) 'down]
    [(and (not (p2? x)) (p1? x)) 'up]
    [(and (not (p2? x)) (not (p1? x))) 'right]))

;;(b)

(define(q1b x)
  (cond
    [(and (p1? x) (p2? x) (p1? (+ x 1))) 'up]
    [(and (p1? x) (p2? x) (p1? (* 2 x))) 'down]
    [(and (p1? x) (p2? x) (not (p1? (+ x 1))) (not (p2? (* 2 x)))) 'right]
    [(and (p1? x) (not (p2? x)) (p2? 2)) 'down]
    [(and (p1? x) (not (p2? x)) (not (p2? 2))) 'up]
    [(and (not (p1? x)) (p1? 0) (p2? x)) 'left]
    [(and (not (p1? x)) (p1? 0) (not (p2? x))) 'right]
    [(and (not (p1? x)) (not (p1? 0))) 'down]))

;;(c)

(define(q1c x)
  (cond
    [(and (p1? x) (p2? x)) 'up]
    [(and (not (p1? x)) (p2? x)) 'up]
    [(and (p1? x) (not (p2? x))) 'up]
    [(and (not (p1? x)) (not (p2? x))) true]))