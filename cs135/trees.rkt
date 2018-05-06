;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname trees) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tingda Du  (20719637)
;; CS135 Fall 2017
;; Assignment 06, Problem 3
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-struct node(key val left right))
;; A Node is a (make-node Num Str BT BT)

(define exampleBT
  (make-node 1 "a"
             (make-node 7 "b" empty empty)
             (make-node 3 "c" (make-node 7 "d" empty empty) empty)))

;; (a)
;; (height bt) produces the height of the tree
;; (height: BT -> Nat)
;; Examples:
(check-expect (height exampleBT) 3)
(check-expect (height empty) 0)

(define(height bt)
  (cond
    [(empty? bt) 0]
    [else (+ 1 (max (height (node-left bt))
                    (height (node-right bt))))]))

;; Tests:
(check-expect (height (make-node 1 "a"
                                 (make-node 7 "b" empty empty)
                                 (make-node 3 "c" (make-node 7 "d"
                                                             (make-node 2 "k" empty empty) empty) empty))) 4)
(check-expect (height (make-node 11 "f" empty empty)) 1)


;; (b)
;; (find-in-tree node symbol) either produces the key which is in the node rooted
;;   at the tree after following those movements, starting at the root of the
;;   tree, or, if that path goes beyond a leaf in the tree, the function should
;;   produce false.
;; (find-in-tree: BT Sym -> (Anyof Nat false)
;; Examples:
(check-expect (find-in-tree exampleBT '(R L)) 7)
(check-expect (find-in-tree exampleBT empty) 1)
(check-expect (find-in-tree empty '(R L R L)) false)

(define (find-in-tree node symbol)
  (cond
    [(empty? node) false]
    [(empty? symbol) (node-key node)]
    [(symbol=? (first symbol) 'R) (find-in-tree (node-right node) (rest symbol))]
    [(symbol=? (first symbol) 'L) (find-in-tree (node-left node) (rest symbol))]))

;; Tests:
(check-expect (find-in-tree exampleBT '(L L)) false)
(check-expect (find-in-tree exampleBT '(L)) 7)
(check-expect (find-in-tree exampleBT '(R)) 3)


;; (c)
;; (prune bt n) produces the binary tree where all subtrees rooted at the consumed
;;   number n (as a key) have been removed from the consumed binary tree
;; (prune: BT Num -> BT)
(check-expect (prune exampleBT 1) empty)
(check-expect (prune empty 11) empty)
(check-expect (prune exampleBT 7)
              (make-node 1 "a" empty (make-node 3 "c" empty empty)))

(define (prune node n)
  (cond
    [(empty? node) empty]
    [(= n (node-key node)) empty]
    [else (make-node (node-key node) (node-val node)
                     (prune (node-left node) n)
                     (prune (node-right node) n))]))

;; Tests:
(check-expect (prune exampleBT 3)
              (make-node 1 "a" (make-node 7 "b" empty empty) empty))


                      