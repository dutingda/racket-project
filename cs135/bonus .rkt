;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |bonus |) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 02, Problem 5(bonus)
;;***************************************************
;;

;; (a)
;; (can-donate-to/bonus? donor-blood-type recipient-blood-type) produce whether the donor's
;;   blood type is acceptable for the recipient's blood type
;; (can-donate-to/bonus?: Sym Sym -> Bool)
;; requires: the blood type symbol must be one of these: O-, O+, A-, A+, B-, B+, AB-, AB+
;; Example:

(check-expect (can-donate-to/bonus? 'AB- 'B-) false)

(define(can-donate-to/bonus? donor-blood-type recipient-blood-type)
  (and(not(and (symbol=? donor-blood-type 'O+) (symbol=? recipient-blood-type 'O-)))
      (not(and (symbol=? donor-blood-type 'O+) (symbol=? recipient-blood-type 'A-)))
      (not(and (symbol=? donor-blood-type 'O+) (symbol=? recipient-blood-type 'B-)))
      (not(and (symbol=? donor-blood-type 'O+) (symbol=? recipient-blood-type 'AB-)))
      (not(and (symbol=? donor-blood-type 'A-) (symbol=? recipient-blood-type 'O-)))
      (not(and (symbol=? donor-blood-type 'A-) (symbol=? recipient-blood-type 'O+)))
      (not(and (symbol=? donor-blood-type 'A-) (symbol=? recipient-blood-type 'B-)))
      (not(and (symbol=? donor-blood-type 'A-) (symbol=? recipient-blood-type 'B+)))
      (not(and (symbol=? donor-blood-type 'A+) (symbol=? recipient-blood-type 'O-)))
      (not(and (symbol=? donor-blood-type 'A+) (symbol=? recipient-blood-type 'O+)))
      (not(and (symbol=? donor-blood-type 'A+) (symbol=? recipient-blood-type 'A-)))
      (not(and (symbol=? donor-blood-type 'A+) (symbol=? recipient-blood-type 'B-)))
      (not(and (symbol=? donor-blood-type 'A+) (symbol=? recipient-blood-type 'B+)))
      (not(and (symbol=? donor-blood-type 'A+) (symbol=? recipient-blood-type 'AB-)))
      (not(and (symbol=? donor-blood-type 'B-) (symbol=? recipient-blood-type 'O-)))
      (not(and (symbol=? donor-blood-type 'B-) (symbol=? recipient-blood-type 'O+)))
      (not(and (symbol=? donor-blood-type 'B-) (symbol=? recipient-blood-type 'A-)))
      (not(and (symbol=? donor-blood-type 'B-) (symbol=? recipient-blood-type 'A+)))
      (not(and (symbol=? donor-blood-type 'B+) (symbol=? recipient-blood-type 'O-)))
      (not(and (symbol=? donor-blood-type 'B+) (symbol=? recipient-blood-type 'O+)))
      (not(and (symbol=? donor-blood-type 'B+) (symbol=? recipient-blood-type 'A-)))
      (not(and (symbol=? donor-blood-type 'B+) (symbol=? recipient-blood-type 'A+)))
      (not(and (symbol=? donor-blood-type 'B+) (symbol=? recipient-blood-type 'B-)))
      (not(and (symbol=? donor-blood-type 'B+) (symbol=? recipient-blood-type 'AB-)))
      (not(and (symbol=? donor-blood-type 'AB-) (symbol=? recipient-blood-type 'O-)))
      (not(and (symbol=? donor-blood-type 'AB-) (symbol=? recipient-blood-type 'O+)))
      (not(and (symbol=? donor-blood-type 'AB-) (symbol=? recipient-blood-type 'A-)))
      (not(and (symbol=? donor-blood-type 'AB-) (symbol=? recipient-blood-type 'A+)))
      (not(and (symbol=? donor-blood-type 'AB-) (symbol=? recipient-blood-type 'B-)))
      (not(and (symbol=? donor-blood-type 'AB-) (symbol=? recipient-blood-type 'B+)))
      (not(and (symbol=? donor-blood-type 'AB+) (symbol=? recipient-blood-type 'O-)))
      (not(and (symbol=? donor-blood-type 'AB+) (symbol=? recipient-blood-type 'O+)))
      (not(and (symbol=? donor-blood-type 'AB+) (symbol=? recipient-blood-type 'A-)))
      (not(and (symbol=? donor-blood-type 'AB+) (symbol=? recipient-blood-type 'A+)))
      (not(and (symbol=? donor-blood-type 'AB+) (symbol=? recipient-blood-type 'B-)))
      (not(and (symbol=? donor-blood-type 'AB+) (symbol=? recipient-blood-type 'B+)))
      (not(and (symbol=? donor-blood-type 'AB+) (symbol=? recipient-blood-type 'AB-)))))

;; Tests:

(check-expect(can-donate-to/bonus? 'B+ 'AB+) true)
(check-expect(can-donate-to/bonus? 'B+ 'B-) false)
(check-expect(can-donate-to/bonus? 'B+ 'AB-) false)
(check-expect (can-donate-to/bonus? 'A- 'AB-) true)




      
                            
                                                 
  
