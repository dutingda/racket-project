;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname blood) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 02, Problem 4
;;***************************************************
;;

;; (a)
;; (can-donate-to/cond? donor-blood-type recipient-blood-type) produce whether the donor's
;;   blood type is acceptable for the recipient's blood type
;; (can-donate-to/cond?: Sym Sym -> Bool)
;; requires: the blood type symbol must be one of these: O-, O+, A-, A+, B-, B+, AB-, AB+
;; Example: 

(check-expect (can-donate-to/cond? 'AB- 'B-) false)

(define (can-donate-to/cond? donor-blood-type recipient-blood-type)
  (cond
    [(symbol=? donor-blood-type 'O-) true]
    [(symbol=? recipient-blood-type 'AB+) true]
    [(symbol=? donor-blood-type recipient-blood-type) true]
    [(symbol=? donor-blood-type 'O+)
     (cond
       [(symbol=? recipient-blood-type 'A+) true]
       [(symbol=? recipient-blood-type 'B+) true]
       [else false])]
    [(symbol=? donor-blood-type 'A-)
     (cond
       [(symbol=? recipient-blood-type 'A+) true]
       [(symbol=? recipient-blood-type 'AB-) true]
       [else false])]
    [(symbol=? donor-blood-type 'B-)
     (cond
       [(symbol=? recipient-blood-type 'B+) true]
       [(symbol=? recipient-blood-type 'AB-) true]
       [else false])]
    [else false]))
   
    
;; Tests:

(check-expect(can-donate-to/cond? 'B- 'AB+) true)
(check-expect(can-donate-to/cond? 'O+ 'A+) true)
(check-expect(can-donate-to/cond? 'AB+ 'AB-) false)

;; (b)
;; (can-donate-to/bool? donor-blood-type recipient-blood-type) produce whether the donor's
;;   blood type is acceptable for the recipient's blood type
;; (can-donate-to/bool?: Sym Sym -> Bool)
;; requires: the blood type symbol must be one of these: O-, O+, A-, A+, B-, B+, AB-, AB+
;; Example:

(check-expect(can-donate-to/bool? 'A- 'AB+) true)

(define(can-donate-to/bool? donor-blood-type recipient-blood-type)
  (or (symbol=? donor-blood-type 'O-)
      (symbol=? recipient-blood-type 'AB+)
      (symbol=? donor-blood-type recipient-blood-type)
      (and (symbol=? donor-blood-type 'O+) (or (symbol=? recipient-blood-type 'A+)
                                               (symbol=? recipient-blood-type 'B+)))
      (and (symbol=? donor-blood-type 'A-) (or (symbol=? recipient-blood-type 'A+)
                                               (symbol=? recipient-blood-type 'AB-)))
      (and (symbol=? donor-blood-type 'B-) (or (symbol=? recipient-blood-type 'B+)
                                               (symbol=? recipient-blood-type 'AB-)))))
                                               
 

;; Tests:

(check-expect(can-donate-to/bool? 'B+ 'AB+) true)
(check-expect(can-donate-to/bool? 'B+ 'B-) false)
(check-expect(can-donate-to/bool? 'B+ 'AB-) false)
(check-expect (can-donate-to/bool? 'A- 'AB-) true)

                                                

      
    
       

       