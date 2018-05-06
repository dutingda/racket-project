;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname nutrition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;
;;***************************************************
;; Tingda Du (20719637)
;; CS 135 Fall 2017
;; Assignment 03, Problem 2
;;***************************************************
;;

;; Useful Constants
(define cal-per-gram-fat 9)
(define cal-per-gram-carbs 4)
(define cal-per-gram-protein 4)


(define-struct nutri-fact (name serving fat carbs sugar protein))
;; A Nutri-Fact is a (make-nutri-fact Str Num Num Num Num Num))
;;requires: 0 < serving
;; fat + carbbs + protein <= serving
;; 0 <= sugar <= carbs
;; 0 <= fats, protein


;; my-nutri-fact-fn: Nutri-Fact -> Any
;; (define(my-nutri-fact-fn food)
;;   (... (nutri-fact-name food)...
;;        (nutri-fact-serving food)...
;;        (nutri-fact-fat food)...
;;        (nutri-fact-carbs food)...
;;        (nutri-fact-sugar food)...
;;        (nutri-fact-protein food)...))



;; (resize nutri-old new-serving-size) resize consumes a Nutri-Fact and a new
;;    serving size (a positive number) and produces a new Nutri-Fact
;;    with the new serving size
;; (resize: Nutri-Fact Num -> Nutri-Fact)
;;   requires: new-seving-size > 0
;; Example:
(check-expect(resize
              (make-nutri-fact "Honey Nut Cheerios" 29 1.5 23 9 2) 58)
             (make-nutri-fact "Honey Nut Cheerios" 58 3 46 18 4))

(define(resize nutri-old new-serving-size)
  (make-nutri-fact (nutri-fact-name nutri-old)
                   new-serving-size
                   (* (/ new-serving-size (nutri-fact-serving nutri-old))
                      (nutri-fact-fat nutri-old))
                   (* (/ new-serving-size (nutri-fact-serving nutri-old))
                      (nutri-fact-carbs nutri-old))
                   (* (/ new-serving-size (nutri-fact-serving nutri-old))
                      (nutri-fact-sugar nutri-old))
                   (* (/ new-serving-size (nutri-fact-serving nutri-old))
                      (nutri-fact-protein nutri-old))))

;;Tests:
(check-expect(resize
              (make-nutri-fact "Ostrich, raw, outside strip" 100 2.2 0 0 23) 150)
             (make-nutri-fact "Ostrich, raw, outside strip" 150 3.3 0 0 34.5))
(check-expect(resize
              (make-nutri-fact "Wheat, cooked, KAMUT khorasan" 100 0.8 28 3.1 5.7) 50)
             (make-nutri-fact "Wheat, cooked, KAMUT khorasan" 50 0.4 14 1.55 2.85))


;; (calories food-nutri-fact) produces the number of calories there are in the
;;   particular serving in the food by using the food-nutri-fact, which is the
;;   the nutrition fact of the food
;; (resize: Nutri-Fact -> Num)
;; Example:
(check-expect(calories (make-nutri-fact "McDONALD'S, QUARTER POUNDER" 100 12 22 5.1 14))
             252)


(define(calories food-nutri-fact)
  (+ (* (nutri-fact-fat food-nutri-fact) cal-per-gram-fat)
     (* (nutri-fact-carbs food-nutri-fact) cal-per-gram-carbs)
     (* (nutri-fact-protein food-nutri-fact) cal-per-gram-protein)))


;; Tests:
(check-expect(calories (make-nutri-fact "MORI-NU, lite firm, silken, Tofu" 300 2.4 3.3 1 18.9))
             110.4)
(check-expect(calories (make-nutri-fact "MURRAY, Lemon Creme Sandwich Cookies" 250 47.5 185 95 11.75))
             1214.5)


;; (choose-for-diet first-diet second-diet) produces the one that is the most
;;    appropriate for your friend's diet
;; (choose-for-diet: Nutri-Fact Nutri-Fact -> Nutri-Fact)
;; Example:
(check-expect(choose-for-diet
              (make-nutri-fact "Muffins" 100 8.4 51 18 5.9)
              (make-nutri-fact "Tennis Bread" 250 2.75 132.5 7.5 22.5))
             (make-nutri-fact "Tennis Bread" 250 2.75 132.5 7.5 22.5))


(define(choose-for-diet first-diet second-diet)
  (cond
    [(< (/ (nutri-fact-sugar first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-sugar second-diet) (nutri-fact-serving second-diet)))
     first-diet]
    [(> (/ (nutri-fact-sugar first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-sugar second-diet) (nutri-fact-serving second-diet)))
     second-diet]
    [(> (/ (nutri-fact-protein first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-protein second-diet) (nutri-fact-serving second-diet)))
     first-diet]
    [(< (/ (nutri-fact-protein first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-protein second-diet) (nutri-fact-serving second-diet)))
     second-diet]
    [(< (/ (nutri-fact-carbs first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-carbs second-diet) (nutri-fact-serving second-diet)))
     first-diet]
    [(> (/ (nutri-fact-carbs first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-carbs second-diet) (nutri-fact-serving second-diet)))
     second-diet]
    [(< (/ (nutri-fact-fat first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-fat second-diet) (nutri-fact-serving second-diet)))
     first-diet]
    [(> (/ (nutri-fact-fat first-diet) (nutri-fact-serving first-diet))
        (/ (nutri-fact-fat second-diet) (nutri-fact-serving second-diet)))
     second-diet]
    [else first-diet]))


;; Tests:
(check-expect(choose-for-diet(make-nutri-fact "Zwieback" 100 9.7 74 13 10)
                             (make-nutri-fact "Zwieback edition 2" 200 30 148 26 20))
             (make-nutri-fact "Zwieback" 100 9.7 74 13 10))
(check-expect(choose-for-diet(make-nutri-fact "Yogurt, whole milk, plain, Greek" 100 5 4 4 9)
                             (make-nutri-fact "Yogurt, whole milk, plain, Greek edition 2" 25 2 3 1 3))
             (make-nutri-fact "Yogurt, whole milk, plain, Greek edition 2" 25 2 3 1 3))
(check-expect(choose-for-diet(make-nutri-fact "Shoshone Bannock" 100 2.9 0 0 31)
                             (make-nutri-fact "Shoshone Bannock edition 2" 270 8 2 0 83.7))
             (make-nutri-fact "Shoshone Bannock" 100 2.9 0 0 31))
(check-expect(choose-for-diet(make-nutri-fact "Radicchio, raw" 100 0.3 4.5 0.6 1.4)
                             (make-nutri-fact "Radicchio, raw edition 2" 300 0.9 13.5 1.8 4.2))
             (make-nutri-fact "Radicchio, raw" 100 0.3 4.5 0.6 1.4))
(check-expect(choose-for-diet
              (make-nutri-fact "Tennis Bread" 250 2.75 132.5 7.5 22.5)
              (make-nutri-fact "Muffins" 100 8.4 51 18 5.9))
             (make-nutri-fact "Tennis Bread" 250 2.75 132.5 7.5 22.5))
(check-expect(choose-for-diet
              (make-nutri-fact "Vital wheat gluten" 100 1.9 14 0 75)
              (make-nutri-fact "Vital wheat gluten edition 2" 200 4 10 0 150))
             (make-nutri-fact "Vital wheat gluten edition 2" 200 4 10 0 150))
(check-expect(choose-for-diet
              (make-nutri-fact "Chewing gum pro" 100 0.3 97 66 2.5)
              (make-nutri-fact "Chewing gum" 400 1.2 400 264 0))
             (make-nutri-fact "Chewing gum pro" 100 0.3 97 66 2.5))
(check-expect(choose-for-diet
              (make-nutri-fact "Pastrami, turkey" 100 6.2 3.3 3.3 16)
              (make-nutri-fact "Pastrami, turkey low fat" 300 18 9.9 9.9 48))
             (make-nutri-fact "Pastrami, turkey low fat" 300 18 9.9 9.9 48))


;; (valid-nutri-fact? value) produces true if it is a valid Nutri-Fact
;;   according to the provided data definition above by consuming any
;;   random value, otherwise it produces false. It always produces booleans
;; (valid-nutri-fact?: Any -> Bool)
;; Example:
(check-expect(valid-nutri-fact? (make-nutri-fact "Jackfruit, syrup pack, canned" 20 -3 80 790 4)) false)


(define(valid-nutri-fact? value)
  (cond
    [(not (nutri-fact? value)) false]
    [else (cond
            [(and (< 0 (nutri-fact-serving value))
                  (<= (+ (nutri-fact-fat value)
                         (nutri-fact-carbs value)
                         (nutri-fact-protein value))
                      (nutri-fact-serving value))
                  (>= (nutri-fact-sugar value) 0)
                  (<= (nutri-fact-sugar value)
                      (nutri-fact-carbs value))
                  (>= (nutri-fact-fat value) 0)
                  (>= (nutri-fact-protein value) 0)) true]
            [else false])]))


;; Tests:
(check-expect(valid-nutri-fact? (make-nutri-fact "Apple juice" -2 3 4 2 9)) false)
(check-expect(valid-nutri-fact? (make-nutri-fact "Roasted duck" 100 27 34 70 39)) false)
(check-expect(valid-nutri-fact? (make-nutri-fact "cheese pizza" 200 69 85 85 46)) true)
(check-expect(valid-nutri-fact? (make-posn 7.90 84392)) false)
(check-expect(valid-nutri-fact? (make-nutri-fact "popcorn" 200 98 90 58 13)) false)
(check-expect(valid-nutri-fact? (make-nutri-fact "popcorn" 0 3 4 8 0.7)) false)
(check-expect(valid-nutri-fact? (make-nutri-fact "ketchup" 200 98 102 72 -13)) false)



    



