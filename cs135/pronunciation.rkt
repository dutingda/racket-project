;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname pronunciation) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 05, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ask Racket to give us access to the code in this file.
;; Do not remove this line.
(require "pronunciationlib.rkt")

;; The data definitions as given in the question.

;; A Vowel is a (list Sym (anyof 0 1 2))

;; A Phoneme is an (anyof Sym Vowel)

;; A Pronunciation is a (listof Phoneme)
;; requires: the list contains exactly one vowel with a stress of 1

;; A Dictionary is a (listof (list Str Pronunciation))
;; requires: The strings in each sub-list appear in alphabetical
;;           order in the Dictionary.

;; --------------------------
;; Place your functions here!
;; --------------------------

;; (a)
;; (pronunciation string dictionary) produces the Pronunciation of the string in the
;;   dictionary or empty if it is not in the dictionary
;; (pronunciation: Str Dictionary -> Pronunciation)
;; Example:
(check-expect (pronunciation "ugly" toy-dictionary) '((AH 1) G L (IY 0)))
(check-expect (pronunciation "abc" empty) empty)
(check-expect (pronunciation "abc" toy-dictionary) empty)

(define (pronunciation string dictionary)
  (cond
    [(empty? dictionary) empty]
    [(string=? string (first (first dictionary)))
     (second (first dictionary))]
    [else
     (pronunciation string (rest dictionary))]))
     
;; (num pronunciation) produces a natural number saying how may syllables
;;   are in the consumed pronunciation
;; (num: Pronunciation -> Nat)
;; Examples:
(check-expect (num empty) 0)
(check-expect (num '((AH 1) G L (IY 0))) 2)

(define (num pronunciation)
  (cond
    [(empty? pronunciation) 0]
    [(cons? (first pronunciation))
     (+ 1 (num (rest pronunciation)))]
    [else
     (num (rest pronunciation))]))

;; (num-syllables string dictionary) produces a natural number saying how many syllables
;;   are in the string (the given word), according to the consumed dictionary
;; (num-syllables: Str Dictionary -> Nat)
;; Examples:
(check-expect (num-syllables "ugly" toy-dictionary) 2)

(define (num-syllables string dictionary)
  (num (pronunciation string dictionary)))

;; Tests:
(check-expect (num-syllables "abc" empty) 0)
(check-expect (num-syllables "abs" toy-dictionary) 0)
(check-expect (num-syllables "cook" toy-dictionary) 1)
(check-expect (num-syllables "effectively" toy-dictionary) 4)


;; (b)
;; (get-pattern pronunciation) produces a list of natural numbers according to the
;;   pronunciation provided, which represents the pattern of the stress.
;; (get-pattern: Pronunciation -> (listof Nat))
;; Examples:
(check-expect (get-pattern '(K (AH 0) N K R (IY 1) T)) '(0 1))

(define (get-pattern pronunciation)
  (cond
    [(empty? pronunciation) empty]
    [(cons? (first pronunciation))
     (cons (second (first pronunciation))
           (get-pattern (rest pronunciation)))]
    [else
     (get-pattern (rest pronunciation))]))

;; (find-stress-pattern lon dictionary) produces a list of all those words in the
;;   "dictionary" whose stress pattern (the numbers assciated with its vowels, in order)
;;   is equal to the given pattern
;; (find-stress-pattern (listof Nat) Dictionary -> (listof String))
;; Examples:
(check-expect (find-stress-pattern '(0 1) toy-dictionary)
              (list "adopt" "concrete" "deprive" "describe" "petite"))


(define (find-stress-pattern lon dictionary)
  (cond
    [(empty? dictionary) empty]
    [(equal? lon (get-pattern (second (first dictionary))))
     (cons (first (first dictionary))
           (find-stress-pattern lon (rest dictionary)))]
    [else
     (find-stress-pattern lon (rest dictionary))]))

;; Tests
(check-expect (find-stress-pattern '(1 0) toy-dictionary)
                                   (list "actress" "awful" "billion" "smugly" "ugly"))
(check-expect (find-stress-pattern empty toy-dictionary) empty)
(check-expect (find-stress-pattern '(1 2 0) empty) empty)


;; (c)

;; (find-rhyme pronunciation) produces the rhyme given the list of stress pattern
;; (find-rhyme: Pronunciation -> Pronunciation
;; Examples:
(check-expect (find-rhyme '(K (AH 0) N K R (IY 1) T)) '((IY 1) T))
(check-expect (find-rhyme empty) empty)


(define (find-rhyme pronunciation)
  (cond
    [(empty? pronunciation) empty]
    [(and (cons? (first pronunciation))
          (= 1 (second (first pronunciation)))) pronunciation]
    [else
     (find-rhyme (rest pronunciation))]))


;; (get-word rhyme word dictionary) produces the list of words with same rhyme
;;   in the dictionary given the rhyme of the word, but also not equal to word
;; (get-word: (listof Phoneme) Str Dictionary -> (listof Str)
;; Examples:
(check-expect (get-word '((IY 1) T) "concrete" toy-dictionary) (list "petite"))
(check-expect (get-word empty "abc" toy-dictionary) empty)
              
(define (get-word rhyme word dictionary)
  (cond
    [(empty? dictionary) empty]
    [(and (equal? rhyme
                  (find-rhyme (second (first dictionary))))
          (not (string=? word (first (first dictionary)))))    
     (cons (first (first dictionary)) (get-word rhyme word (rest dictionary)))]
    [else
     (get-word rhyme word (rest dictionary))]))


;; (find-rhymes String Dictionary) produces a list of strings, in alphabetical order
;;   consisting of all words in the dictionary that rhyme with the given word, not
;;   including the word itself
;; (find-rhymes Str Dictionary -> (listof Str)
;; Examples:
(check-expect (find-rhymes "pigeon" cmudict)
              (list "bijan" "pridgen" "religion" "smidgen"))


(define (find-rhymes word dictionary)
  (get-word (find-rhyme (pronunciation word dictionary)) word dictionary))

;; Tests:
(check-expect (find-rhymes "concrete" toy-dictionary )
              (list "petite"))
            




   
   
   
                        
              

         
  

     
    
  
  
  