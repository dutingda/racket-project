;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname anagrams) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 06, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (a)
;; (insert-chars c loc) produces a list that c was inserted in a sorted list loc
;; (insert-chars: Char (listof Char) -> (listof Char))
;; Examples:
(check-expect (insert-chars #\e (string->list "abcdf")) '(#\a #\b #\c #\d #\e #\f))
(check-expect (insert-chars #\l (string->list "cjosvxz")) (string->list "cjlosvxz"))

(define (insert-chars c loc)
  (cond
    [(empty? loc) (cons c empty)]
    [(char<=? c (first loc))
     (cons c loc)]
    [else
     (cons (first loc) (insert-chars c (rest loc)))]))
     
;; (sort-chars cl) produces a sorted list (in increasing order) containing exactly
;;   those characters in cl
;; (sort-chars: (listof Char) -> (listof Char))
;; Examples:
(check-expect (sort-chars '(#\c #\a #\z #\w #\a #\y))
              '(#\a #\a #\c #\w #\y #\z))
(check-expect (sort-chars empty) empty)


(define (sort-chars cl)
  (cond
    [(empty? cl) empty]
    [else
     (insert-chars (first cl) (sort-chars (rest cl)))]))


;; Tests:
(check-expect (sort-chars (string->list "taxBTnc"))
                          (string->list "BTacntx"))


;; (b)
;; (anagrams/sort? str1 str2) produces true if the str1 and str2 are anagrams of each
;;   other, and false otherwise
;; (anagrams/sort? Str Str -> Bool)
;; Example:
(check-expect (anagrams/sort? "str" "rts") true)

(define (anagrams/sort? str1 str2)
  (equal? (sort-chars (string->list str1))
          (sort-chars (string->list str2))))


;; Tests:
(check-expect (anagrams/sort? "ptsabrtaK" "ratKsabtp") true)
(check-expect (anagrams/sort? "fsdfsejk" "fjkeekwie") false)


;; (c)

;; (add1-to-key k al) produces an list which comsumed k that is in the
;;   original list al and add 1 to the corresponding value for k, if there is not such
;;   key, return list combined with a list contain k and 1
;; (add1-to-key: Nat -> (listof (list Any Nat))
;; Examples:
(check-expect (add1-to-key '(3 0) (list (list 'red 7) (list 9 20) (list '(3 0) 1)))
              (list (list 'red 7) (list 9 20) (list '(3 0) 2)))
(check-expect (add1-to-key 'red (list (list 'red 7) (list 9 20) (list '(3 0) 1)))
                           (list (list 'red 8) (list 9 20) (list '(3 0) 1)))
(check-expect (add1-to-key #\m (list (list #\m 10))) (list (list #\m 11)))
(check-expect (add1-to-key "c" (list (list 'a 2) (list #\t 8)))
              (list (list 'a 2) (list #\t 8) (list "c" 1)))
              
(define (add1-to-key k al)
  (cond
    [(empty? al) (cons (list k 1) empty)]
    [(equal? (first (first al)) k)
     (cons (list (first (first al)) (add1 (second (first al)))) (rest al))]
    [else
     (cons (first al) (add1-to-key k (rest al)))]))
 

;; (freq-count-help l al) produces the list of pairs, where the first element of each
;;   pair is an element from the list and the second element of each pair is the number
;;   of times that element appeared in the consumed list, al is the accumulator
;; (freq-count-help: (listof Any) (listof (list Any Nat)) -> (listof (list Any Nat))
;; Examples:
(check-expect (freq-count-help '(red 7 9 (7 9) red 9) empty)
              '((red 2) (7 1) (9 2) ((7 9) 1)))
(check-expect (freq-count-help '(red 2 7 (make-posn 6 2) 7 "word" "word" 7 "m" #\t)
                               empty)
              '((red 1) (2 1) (7 3) ((make-posn 6 2) 1) ("word" 2) ("m" 1) (#\t 1)))
(check-expect (freq-count-help empty empty) empty)


(define (freq-count-help l al)
  (cond
    [(empty? l) al]
    [else
     (freq-count-help (rest l) (add1-to-key (first l) al))]))

;; (freq-count loa) produces the list of pairs, where the first element of each
;;   pair is an element from the list and the second element of each pair is the number
;;   of times that element appeared in the consumed list
;; (freq-count (listof Any) -> (listof (list Any Nat))
;; Example:
(check-expect (freq-count '(red 7 9 (7 9) red 9)) '((red 2) (7 1) (9 2) ((7 9) 1)))


(define (freq-count loa)
  (freq-count-help loa empty))


;; Tests:
(check-expect (freq-count '(red 2 7 (make-posn 6 2) 7 "word" "word" 7 "m" #\t))
              '((red 1) (2 1) (7 3) ((make-posn 6 2) 1) ("word" 2) ("m" 1) (#\t 1)))


;; (d)

;; (in? elem list) produces true if the elem is in the list, false otherwise
;; (in?: Any (listof Any) -> Bool)
;; Examples:
(check-expect (in? "fjdk" empty) false)
(check-expect (in? 'pck (list 'pck "a" (make-posn #\d "k"))) true)
(check-expect (in? "abc" (list "a" "b" "c")) false)
(check-expect (in? '(red 5) '((blue 9) (red 5) ("string" 0))) true)


(define (in? elem list)
  (cond
    [(empty? list) false]
    [else
     (or (equal? elem (first list))
         (in? elem (rest list)))]))

;; (freq-equiv-help? list1 list2) determines if they list1 are rearrangements of list2
;;    when list1 and list2 have the same length
;; (freq-equiv-help?: (listof (list Any Nat)) (listof (list Any Nat)) -> Bool)
;; Examples:
(check-expect (freq-equiv-help? empty empty) true)
(check-expect (freq-equiv-help? '((red 5) (blue 6)) '((blue 9) (red 5) ("string" 0)))
              false)


(define (freq-equiv-help? list1 list2)
  (cond
    [(empty? list1) true]
    [else
     (and (in? (first list1) list2)
          (freq-equiv-help? (rest list1) list2))]))


;; (freq-equiv? list1 list2) determines if they list1 are rearrangements of list2
;; (freq-equiv? (listof (list Any Nat)) (listof (list Any Nat)) -> Bool)
;; Examples:
(check-expect (freq-equiv? '((blue 5)) '((red 4) (blue 5))) false)
(check-expect (freq-equiv? empty '((blue 5))) false)
(check-expect (freq-equiv? '((red 5) (blue 5)) '((blue 5) (red 5))) true)

(define (freq-equiv? list1 list2)
  (cond
    [(= (length list1) (length list2))
     (freq-equiv-help? list1 list2)]
    [else false]))

;; Tests:
(check-expect (freq-equiv? '((red 5) (blue 6)) '((blue 9) (red 5) ("string" 0))) false)
(check-expect (freq-equiv-help? empty empty) true)
(check-expect (freq-equiv? '((red 5) (blue 9) ("string" 0)) '((blue 9) (red 5)
                                                                       ("string" 0)))
              true)
                           
;; (e)
;; (anagrams/count? str1 str2) produces true if the two strings are anagrams of each
;;   other, and false otherwise
;; (anagrams/count? Str Str -> Bool)
;; Example:
(check-expect (anagrams/count? "abttac" "acabtt") true)

(define (anagrams/count? str1 str2)
  (freq-equiv? (freq-count (string->list str1))
               (freq-count (string->list str2))))


;; Tests:
(check-expect (anagrams/count? "tabccd" "batccd") true)
(check-expect (anagrams/count? "ppc" "cppa") false)
(check-expect (anagrams/count? "" "") true)

               
               
                           
                           

 
                          