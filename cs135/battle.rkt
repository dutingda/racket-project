;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname battle) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 03, Problem 4
;;***************************************************
;;

;; Useful Constants
(define red 'red)
(define yellow 'yellow)
(define green 'green)
(define blue 'blue)
(define brown 'brown)
(define purple 'purple)

(define-struct card (strength colour))
;; A Card is a (make-card Nat Sym)
;; requires: 1<= strength <= 9
;;           color is one of: 'red 'yellow 'greeen 'blue 'purple 'brown


(define-struct hand (c1 c2 c3))
;; A Hand is a (make-hand Card Card Card)


;; (colour-judge? my-hand) determine whether the hand has three Cards that have the
;;   same colour
;; (colour-judge: Hand -> Bool)
;; Example:
(check-expect (colour-judge? (make-hand (make-card 6 brown)
                                        (make-card 3 brown)
                                        (make-card 9 brown))) true)


(define(colour-judge? my-hand)
  (and (symbol=? (card-colour (hand-c1 my-hand)) (card-colour (hand-c2 my-hand)))
       (symbol=? (card-colour (hand-c2 my-hand)) (card-colour (hand-c3 my-hand)))))


;; (run-judge? my-hand) determine whether the hand is when the three strength of the Cards
;;   in "my-hand"(Hand) are sequential(A run can appear in the Hand in any order)
;; (run-judge? Hand -> Bool)
;; Example:
(check-expect (run-judge? (make-hand (make-card 8 brown)
                                     (make-card 7 red)
                                     (make-card 9 green))) true)


(define(run-judge? my-hand)
  (= (+ 1 (min (card-strength (hand-c1 my-hand))
               (card-strength (hand-c2 my-hand))
               (card-strength (hand-c3 my-hand))))
     (max (card-strength (hand-c1 my-hand))
          (card-strength (hand-c2 my-hand))
          (card-strength (hand-c3 my-hand)))))


;; (three-of-a-kind-judge? my-hand) determine whethter the Hand of "my hand" has the same strength of
;;    Cards and their colours are not relevant
;; (three-of-a-kind-judge?: Hand -> Bool)
;; Example:
(check-expect (three-of-a-kind-judge? (make-hand (make-card 2 green)
                                                 (make-card 2 red)
                                                 (make-card 2 yellow))) true)


(define(three-of-a-kind-judge? my-hand)
  (and (= (card-strength (hand-c1 my-hand)) (card-strength (hand-c2 my-hand)))
       (= (card-strength (hand-c2 my-hand)) (card-strength (hand-c3 my-hand)))))


;; (attribute-judge my-hand) produces the attribute of a card into numberes by
;;    using the "my-hand" providing
;; (attribute-judge: Hand -> Num)
;; Example:
(check-expect (attribute-judge (make-hand (make-card 5 brown)
                                          (make-card 6 red)
                                          (make-card 4 green))) 115)

(check-expect (attribute-judge (make-hand (make-card 7 brown)
                                          (make-card 2 red)
                                          (make-card 6 green))) 15)

(check-expect (attribute-judge (make-hand (make-card 8 red)
                                          (make-card 6 red)
                                          (make-card 7 red))) 100021)


(define(attribute-judge my-hand)
  (cond
    [(and (run-judge? my-hand) (colour-judge? my-hand))
     (+ 100000 (+ (card-strength (hand-c1 my-hand))
                  (card-strength (hand-c2 my-hand))
                  (card-strength (hand-c3 my-hand))))]
    [(three-of-a-kind-judge? my-hand)
     (+ 10000 (+ (card-strength (hand-c1 my-hand))
                 (card-strength (hand-c2 my-hand))
                 (card-strength (hand-c3 my-hand))))]
    [(colour-judge? my-hand)
     (+ 1000 (+ (card-strength (hand-c1 my-hand))
                (card-strength (hand-c2 my-hand))
                (card-strength (hand-c3 my-hand))))]
    [(run-judge? my-hand)
     (+ 100 (+ (card-strength (hand-c1 my-hand))
               (card-strength (hand-c2 my-hand))
               (card-strength (hand-c3 my-hand))))]
    [else (+ (card-strength (hand-c1 my-hand))
             (card-strength (hand-c2 my-hand))
             (card-strength (hand-c3 my-hand)))]))


;; (battle first-hand second) produces 'player1 if the either te first Hand defeats the
;;    second Hand or there is a tie. Otherwise, it produces 'player2
;; (battle: Hand Hand -> Sym)
;; Example:
(check-expect(battle (make-hand (make-card 6 brown)
                                (make-card 3 brown)
                                (make-card 9 brown))
                     (make-hand (make-card 3 green)
                                (make-card 3 red)
                                (make-card 3 yellow)))
             'player2)


(define(battle first-hand second-hand)
  (cond
    [(>= (attribute-judge first-hand)
         (attribute-judge second-hand)) 'player1]
    [else 'player2]))

;; Tests:
(check-expect(battle (make-hand (make-card 2 yellow)
                                (make-card 2 green)
                                (make-card 9 red))
                     (make-hand (make-card 3 blue)
                                (make-card 7 brown)
                                (make-card 9 purple)))
             'player2)                               
(check-expect(battle (make-hand (make-card 8 yellow)
                                (make-card 2 green)
                                (make-card 9 red))
                     (make-hand (make-card 3 green)
                                (make-card 6 red)
                                (make-card 9 yellow)))
             'player1)
(check-expect(battle (make-hand (make-card 3 green)
                                (make-card 2 green)
                                (make-card 1 green))
                     (make-hand (make-card 8 green)
                                (make-card 7 green)
                                (make-card 9 green)))
             'player2)
(check-expect(battle (make-hand (make-card 9 yellow)
                                (make-card 9 green)
                                (make-card 9 red))
                     (make-hand (make-card 1 red)
                                (make-card 2 red)
                                (make-card 3 red)))
             'player2)





     
              

              




  
  