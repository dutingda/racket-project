;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname conversion) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 01, Problem 2
;;***************************************************
;;
;; Useful Converters
(define meters-per-mile 1609.344)
(define seconds-per-hour 3600)
(define seconds-per-millifortnight 1209.6)
(define meters-per-smoot 1.7018)
(define liters-per-gallon 3.785411784)
(define hundred-killometers-per-meter 0.00001)

;; (a)
;; (m/s->mph meteric-speed) produces the same speed as original meteric speed
;;   in units of mph
;; (m/s->mph: Num -> Num)
;; Examples:
(check-expect(m/s->mph 1609344) 3600000)

(define(m/s->mph meteric-speed)
  (/ meteric-speed (/ meters-per-mile seconds-per-hour)))

;; Tests:
(check-expect(m/s->mph 0) 0)
(check-expect(m/s->mph -32.18688) -72)


;; (b)
;; (mph->S/mfn imperial-speed) produces the same speed as original imperial-speed
;;   in units of S/mfn
;; (mph->S/mfn: Num -> Num)
;; Examples:
(check-expect(mph->S/mfn 1.7018) 540.739584)
(check-expect(mph->S/mfn 1.352931) 429.88796928)

(define(mph->S/mfn imperial-speed)
  (/ (* (/ (* imperial-speed meters-per-mile) seconds-per-hour) seconds-per-millifortnight) meters-per-smoot))

;; Tests:
(check-expect(mph->S/mfn 0) 0)
(check-expect(mph->S/mfn -12.2053096) -3878.184296448)

;;(c)
;; (mpg->L/100km miles-per-gallon) consumes a fuel efficiency in miles per gallon
;;   and produce the same effciency in units of L/100km
;; (mpg->L/100km: Num -> Num)
;; Requires: miles-per-gallon >= 0
;; Examples:
(check-expect(mpg->L/100km 3) 112903/1440)

(define(mpg->L/100km miles-per-gallon)
  (/ liters-per-gallon (* miles-per-gallon meters-per-mile hundred-killometers-per-meter)))

;; Tests:
(check-expect(mpg->L/100km 2) 112903/960)
(check-expect(mpg->L/100km 2.75) 112903/1320)
