;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname trie) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 07, Problem 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "a07lib.rkt")

;; (a)
(define a-tnode
  (make-tnode
   #\a
   false
   (list (make-tnode #\t true empty))))

(define c-tnode
  (make-tnode
   #\c
   false
   (list
    (make-tnode
     #\o
     false
     (list (make-tnode #\o true empty)
           (make-tnode #\w true empty)))
    (make-tnode
     #\s
     false
     (list
      (make-tnode
       #\1
       false
       (list
        (make-tnode
         #\1
         false
         (list (make-tnode #\5 true empty)
               (make-tnode #\6 true empty)))
        (make-tnode
         #\3
         false
         (list (make-tnode #\5 true empty)
               (make-tnode #\6 true empty))))))))))

(define a-c-trie (make-trie (list a-tnode c-tnode)))
;; (b)
;; trie-template: Trie -> Any
;; (define (trie-template trie)
;;   ...(list-tnode-template (trie-children trie))...)

;; list-tnode-template: (listof TNode) -> Any
;; (define (list-tnode-template lotn)
;;   (cond
;;     [(empty? lotn)...]
;;     [else
;;      (tnode-template (first lotn))...
;;      ...(list-tnode-template (rest lotn))...]))

;; tnode-template: TNode -> Any
;; (define (tnode-template node)
;;   ...(tnode-key node)...
;;   ...(tnode-ends-word? node)...
;;      (list-tnode-template (tnode-children node))...)

;; (c)

;; (in-trie-h1? lotn loc) Produces true if the list of characters is represented
;;   as a word (loc) in the list of TNode "lotn" and false if it is not
;; in-trie-h1?: (listof TNode) Str -> Bool
;; Examples:
(check-expect (in-trie-h1? (list a-tnode c-tnode) (string->list "at")) true)
(check-expect (in-trie-h1? (list a-tnode c-tnode) (string->list "cs1")) false)
(check-expect (in-trie-h1? empty (string->list "cs1")) false)
(check-expect (in-trie-h1? (list a-tnode c-tnode) (string->list "")) false)


(define (in-trie-h1? lotn loc)
  (cond
    [(empty? lotn) false]
    [(empty? loc) false]
    [else
     (or (in-trie-h2? (first lotn) loc)
         (in-trie-h1? (rest lotn) loc))]))

;; (in-trie-h2? node loc) Produces true if the list of character is represented 
;;   a word in the TNode "node" and false otherwise
;; in-trie-h2?: TNode (listof Char) -> Bool
;;   requires: loc is a non-empty list
;; Examples:
(check-expect (in-trie-h2? c-tnode (string->list "cs13")) false)
(check-expect (in-trie-h2? a-tnode (string->list "at")) true)
     
(define (in-trie-h2? node loc)
  (and (char=? (tnode-key node) (first loc))
       (or (and (tnode-ends-word? node) (empty? (rest loc)))
           (in-trie-h1? (tnode-children node) (rest loc)))))

     

;; (in-trie? str trie) Produces true if the string "str" is represented as a word
;;   in the "trie"
;; in-trie?: Str Trie -> Bool
;; Examples:
(check-expect (in-trie? "cs" a-c-trie) false)
(check-expect (in-trie? "" c-d-trie) false)
(check-expect (in-trie? "cower" a-c-trie) false)
(check-expect (in-trie? "cs115" a-c-trie) true)

(define (in-trie? str trie)
  (in-trie-h1? (trie-children trie) (string->list str)))

;; Tests:
(check-expect (in-trie? "dogf" c-d-trie) false)
(check-expect (in-trie? "doz" c-d-trie) false)
(check-expect (in-trie? "dogfish" c-d-trie) true)
(check-expect (in-trie? "abc" c-d-trie) false)
(check-expect (in-trie? "diq" c-d-trie) false)
(check-expect (in-trie? "" c-d-trie) false)
(check-expect (in-trie? "catnip" c-d-trie) true)


;; (d)
;; (list-words-help1 lotn line) Produces a list of all words in the list of TNode
;;    lotn with the accumlator line to keep adding the charactor onto it.
;; list-words-help1: (listof TNode) (listof Char) -> (listof Str)
;; Examples:
(check-expect (list-words-help1 (list a-tnode c-tnode) empty)
              (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))
                    


(define (list-words-help1 lotn line)
  (cond
    [(empty? lotn) empty]
    [else
     (append (list-words-help2 (first lotn) line)
             (list-words-help1 (rest lotn) line))]))


;; (list-words-help2 node line) Produces a list of all words in the TNode node
;;   with the accumlator line to accumulate the character
;; list-words-help2: TNode (listof Char) -> (listof Str)
;; Examples:
(check-expect (list-words-help2 a-tnode empty)
              (list "at"))


(define (list-words-help2 node line)
  (cond
    [(tnode-ends-word? node)
     (cons (list->string (reverse (cons (tnode-key node) line)))
           (list-words-help1 (tnode-children node)
                             (cons (tnode-key node) line)))]
    [else
     (list-words-help1 (tnode-children node)
                       (cons (tnode-key node) line))]))


;; (list-words trie) Produces a list of all the words in the Trie "trie"
;; list-words: Trie -> (listof Str)
;; Examples:
(check-expect (list-words a-c-trie)
              (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136"))

(define (list-words trie)
  (list-words-help1 (trie-children trie) empty))

;; Tests:
(check-expect (list-words c-d-trie)
              (list "cat" "catch" "cater" "catnip" "cattle" "dig" "dog" "dogfish"
                    "donald" "donut" "doze"))
(check-expect (list-words toydict-trie)
              (list "actress" "adopt" "awful" "billion" "blurt" "cold" "concrete"
                    "cook" "deprive" "describe" "dirt" "effectively" "fault"
                    "five" "petite" "publication" "significantly" "smugly"
                    "swapped" "ugly" "wake")) 
        
;; (e)  
;; (insert-word-h1 lotn loc) Produces a list of TNode consisting of the consumed
;;   list of TNode lotn with the word inserted in terms of list of characters loc
;; insert-word-h1: (listof TNode) (listof Char) -> (listof TNode)
;; Examples:
(check-expect (list-words
               (make-trie
                (insert-word-h1 (trie-children a-c-trie) (string->list "cs145"))))
              (list "at" "coo" "cow" "cs115" "cs116" "cs135" "cs136" "cs145"))


(define (insert-word-h1 lotn loc)
  (cond
    [(empty? lotn) (list (insert-word-h2 loc))]
    [(empty? loc) lotn]
    [(char=? (first loc) (tnode-key (first lotn)))
     (cond
       [(not (empty? (rest loc)))
        (append (list
                 (make-tnode
                  (first loc)
                  (tnode-ends-word? (first lotn))
                  (insert-word-h1 (tnode-children (first lotn)) (rest loc))))
                (rest lotn))]
       [else
        (append (list
                 (make-tnode
                  (first loc)
                  true
                  (insert-word-h1 (tnode-children (first lotn)) (rest loc))))
                (rest lotn))])]
    [(char<? (first loc) (tnode-key (first lotn)))
     (cons (insert-word-h2 loc) lotn)]
    [else
     (cons (first lotn) (insert-word-h1 (rest lotn) loc))]))


;; (insert-word-h2 loc) produces the tnode that was inserted--loc, which is
;;   represented in terms of characters
;; insert-word-h2: (listof Char) -> TNode
;; Examples:
(check-expect (insert-word-h2 (string->list "cpx"))
              (make-tnode #\c false (list (make-tnode
                                           #\p
                                           false
                                           (list (make-tnode #\x true empty))))))
(check-expect (insert-word-h2 (string->list "m"))
              (make-tnode #\m true empty))
 
           
(define (insert-word-h2 loc)
  (cond
    [(empty? (rest loc)) (make-tnode (first loc) true empty)]
    [else
     (make-tnode (first loc) false (list (insert-word-h2 (rest loc))))]))
     
  
;; (insert-word str trie) Produces a Trie consisting of the consumed "trie" with
;;   the word inserted
;; insert-word: Str Trie -> Trie
;; Examples:
(check-expect (list-words (insert-word "cowboy" a-c-trie))
              (list "at" "coo" "cow" "cowboy" "cs115" "cs116" "cs135" "cs136"))

(define (insert-word str trie)
  (make-trie (insert-word-h1 (trie-children trie) (string->list str))))
   
;; Tests:
(check-expect (list-words (insert-word "hated" h-u-trie))
              (list "ha" "hat" "hated" "he" "hot" "use"))
(check-expect (list-words (insert-word "ho" h-u-trie))
              (list "ha" "hat" "he" "ho" "hot" "use"))
(check-expect (list-words (insert-word "him" h-u-trie))
              (list "ha" "hat" "he" "him" "hot" "use"))
(check-expect (list-words (insert-word "o" h-u-trie))
              (list "ha" "hat" "he" "hot" "o" "use"))
(check-expect (list-words (insert-word "unbelievable" h-u-trie))
              (list "ha" "hat" "he" "hot" "unbelievable" "use"))
(check-expect (list-words (insert-word "cs" a-c-trie))
              (list "at" "coo" "cow" "cs" "cs115" "cs116" "cs135" "cs136"))

;; (f)
;; (insert-some-words los trie) Produces a Trie consisting of the consumed "trie"
;;   with all of the strings inserted
;; insert-some-words: (listof Str) Trie -> Trie
;; Examples:
(check-expect (list-words (insert-some-words (list "cs137" "cs146" "core" "aim")
                                             a-c-trie))
                          (list "aim" "at" "coo" "core"
                                "cow" "cs115" "cs116" "cs135"
                                "cs136" "cs137" "cs146"))
                                             
 
(define (insert-some-words los trie)
  (cond
    [(empty? los) trie]
    [else
     (insert-some-words (rest los) (insert-word (first los) trie))]))


;; Tests:
(check-expect (list-words (insert-some-words (list "hog" "hoot") h-u-trie))
              (list "ha" "hat" "he" "hog" "hoot" "hot" "use"))
(check-expect (list-words (insert-some-words empty toydict-trie))
              (list "actress" "adopt" "awful" "billion" "blurt" "cold" "concrete"
                    "cook" "deprive" "describe" "dirt" "effectively" "fault"
                    "five" "petite" "publication" "significantly" "smugly"
                    "swapped" "ugly" "wake"))

;; (g)
;; (list-completions-h loc lotn line) Produces the sorted list of strings consisting
;;   of all words in the lotn that begin with prefix contructed as loc with the
;;   help of the accumulator line
;; (listof Char) (listof TNode) (listof Char) -> (listof Str)
;; Examples:
(check-expect (list-completions-h (string->list "cat") (trie-children c-d-trie) empty)
              (list "cat" "catch" "cater" "catnip" "cattle"))


(define (list-completions-h loc lotn line)
  (cond
    [(empty? lotn) empty]
    [(empty? loc) (list-words-help1 lotn line)]
    [(empty? (rest loc))
     (cond
       [(char=? (first loc) (tnode-key (first lotn)))
        (list-words-help2 (first lotn) line)]       
       [else
        (list-completions-h loc (rest lotn) line)])]                                              
    [(char=? (first loc) (tnode-key (first lotn)))
     (list-completions-h (rest loc) (tnode-children (first lotn))
                         (cons (first loc) line))]
    [else
     (list-completions-h loc (rest lotn) line)]))


;; (list-completions str trie) Produces a sorted list of strings consisting of all
;;   words in the trie that begin with that prefix in terms of "str"
;; list-completions: Str Trie -> (listof Str)
;; Examples:
(check-expect (list-completions "cs" a-c-trie) (list "cs115" "cs116" "cs135" "cs136"))


(define (list-completions str trie)
  (list-completions-h (string->list str) (trie-children trie) empty))


;; Tests:
(check-expect (list-completions "don" c-d-trie) (list "donald" "donut"))
(check-expect (list-completions "ha" h-u-trie) (list "ha" "hat"))
(check-expect (list-completions "" h-u-trie) (list "ha" "hat" "he" "hot" "use"))
(check-expect (list-completions "go" h-u-trie) empty)

  
              

                      
              




 
  
  

                 
        
                 
 
