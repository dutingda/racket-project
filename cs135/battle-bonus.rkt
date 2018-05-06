;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname battle-bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 03, Problem 5
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


;; Useful Cards
(define card-1 (make-card 5 red))
(define card-2 (make-card 6 red))
(define card-3 (make-card 7 red))
(define card-4 (make-card 5 green))
(define card-5 (make-card 5 brown))

;; Useful Hands

;; a colour-run
(define hand-1 (make-hand card-1 card-2 card-3))
;; a three-of kinds
(define hand-2 (make-hand card-4 card-5 card-1))
;; a what ever
(define hand-3 (make-hand card-1 card-3 card-5))


;; (run-judge? my-hand) determine whether the hand is when the three strength of the Cards
;;   in "my-hand"(Hand) are sequential(A run can appear in the Hand in any order)
;; (run-judge? Hand -> Bool)
;; Example:
(check-expect (run-judge? (make-hand (make-card 8 brown)
                                     (make-card 7 red)
                                     (make-card 9 green))) true)


(define(run-judge? my-hand)
  (= (+ 2 (min (card-strength (hand-c1 my-hand))
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

(define(not-have-colour? card hand colour)
  (and (not (symbol=? (card-colour (hand-c1 hand))
                      colour))
       (not (symbol=? (card-colour (hand-c2 hand))
                      colour))
       (not (symbol=? (card-colour (hand-c3 hand))
                      colour))
       (not (symbol=? (card-colour card)
                      colour))))

(define(find-colour card hand colour-1 colour-2 colour-3 colour-4 colour-5 colour-6)
  (cond
    [(not-have-colour? card hand colour-1) colour-1]
    [(not-have-colour? card hand colour-2) colour-2]
    [(not-have-colour? card hand colour-3) colour-3]
    [(not-have-colour? card hand colour-4) colour-4]
    [(not-have-colour? card hand colour-5) colour-5]
    [(not-have-colour? card hand colour-6) colour-6]))


(define(find-winner played-card opp-hand)
  (cond
    [(and (not (and (colour-judge? opp-hand)
                    (run-judge? opp-hand)))
          (not (three-of-a-kind-judge? opp-hand)))
     (make-hand played-card
                (make-card (card-strength played-card)
                           (find-colour played-card opp-hand red yellow green blue purple brown))
                (make-card (card-strength played-card)
                           (find-colour played-card opp-hand brown purple blue green yellow red)))]
    [(three-of-a-kind-judge? opp-hand)
     (cond
       [(< (card-strength (hand-c1 opp-hand))
           (card-strength played-card))
        (make-hand played-card
                   (make-card (card-strength played-card)
                              (find-colour played-card opp-hand red yellow green blue purple brown))
                   (make-card (card-strength played-card)
                              (find-colour played-card opp-hand brown purple blue green yellow red)))]
       [else
        (cond
          [(>= (card-strength played-card) 3)
           (make-hand played-card
                      (make-card (- (card-strength played-card) 1) (card-colour played-card))
                      (make-card (- (card-strength played-card) 2) (card-colour played-card)))]
          [(= (card-strength played-card) 2)
           (cond
             [(and (= (card-strength (hand-c1 opp-hand)) 3)
                   (or (symbol=? (card-colour (hand-c1 opp-hand))
                                 (card-colour played-card))
                       (symbol=? (card-colour (hand-c2 opp-hand))
                                 (card-colour played-card))
                       (symbol=? (card-colour (hand-c3 opp-hand))
                                 (card-colour played-card)))) false]
             [else
              (make-hand (make-card 1 (card-colour played-card))
                         (make-card 2 (card-colour played-card))
                         (make-card 3 (card-colour played-card)))])]
          [(= (card-strength played-card) 1)
           (cond
             [(and (or (= (card-strength (hand-c1 opp-hand)) 3)
                       (= (card-strength (hand-c1 opp-hand)) 2))
                   (or (symbol=? (card-colour (hand-c1 opp-hand))
                                 (card-colour played-card))
                       (symbol=? (card-colour (hand-c2 opp-hand))
                                 (card-colour played-card))
                       (symbol=? (card-colour (hand-c3 opp-hand))
                                 (card-colour played-card)))) false]
             [else
              (make-hand (make-card 1 (card-colour played-card))
                         (make-card 2 (card-colour played-card))
                         (make-card 3 (card-colour played-card)))])])])]
    [(and (colour-judge? opp-hand) (run-judge? opp-hand))
     (cond
       [(< (min (card-strength (hand-c1 opp-hand))
                (card-strength (hand-c2 opp-hand))
                (card-strength (hand-c3 opp-hand)))
           (card-strength played-card))
        (cond
          [(<= (+ 2 (card-strength played-card)) 9)
           (make-hand played-card
                      (make-card (+ (card-strength played-card) 1) (card-colour played-card))
                      (make-card (+ (card-strength played-card) 2) (card-colour played-card)))]
          [(or (= (card-strength played-card) 8) (= (card-strength played-card) 9))
           (cond
             [(symbol=? (card-colour (hand-c1 opp-hand)) (card-colour played-card))
              (cond
                [(or (= 7 (max (card-strength (hand-c1 opp-hand))
                               (card-strength (hand-c2 opp-hand))
                               (card-strength (hand-c3 opp-hand))))
                     (= 8 (max (card-strength (hand-c1 opp-hand))
                               (card-strength (hand-c2 opp-hand))
                               (card-strength (hand-c3 opp-hand))))) false]
                [else
                 (make-hand (make-card 7 (card-colour played-card))
                            (make-card 8 (card-colour played-card))
                            (make-card 9 (card-colour played-card)))])]
             [else
              (cond
                [(= 9 (max (card-strength (hand-c1 opp-hand))
                           (card-strength (hand-c2 opp-hand))
                           (card-strength (hand-c3 opp-hand)))) false]
                [else
                 (make-hand (make-card 7 (card-colour played-card))
                            (make-card 8 (card-colour played-card))
                            (make-card 9 (card-colour played-card)))])])])]
       [else false])]))

;;Tests
(check-expect(find-winner (make-card 8 red) hand-1) false)
(check-expect(find-winner (make-card 3 green) hand-2)
             (make-hand (make-card 3 green) (make-card 2 green) (make-card 1 green)))
(check-expect(find-winner (make-card 9 brown)
                          (make-hand (make-card 6 brown) (make-card 7 brown) (make-card 8 brown)))
             false)
(check-expect(find-winner (make-card 9 brown)
                          (make-hand (make-card 6 blue) (make-card 7 blue) (make-card 8 blue)))
             (make-hand (make-card 7 brown) (make-card 8 brown) (make-card 9 brown)))
(check-expect(find-winner (make-card 8 brown)
                          (make-hand (make-card 6 blue) (make-card 7 green) (make-card 8 red)))
             (make-hand (make-card 8 brown) (make-card 8 yellow) (make-card 8 purple)))










        
              
              
               
                           
                           
               
                  
        
        
          
          
          
    
       
       
       
        
        
        
        
             
          