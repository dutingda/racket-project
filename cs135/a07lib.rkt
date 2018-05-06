#lang racket

;; Do not make changes to this file! Your trie.rkt file should include it as
;; follows:
; (require "a07lib.rkt")


;; Include provide and all-defined out per
;; https://stackoverflow.com/questions/8996394/
(provide (struct-out trie)
         (struct-out tnode)
         blank-trie
         c-d-trie
         h-u-trie
         toydict-trie
         struct-out
         provide
         all-defined-out)

;; Note: the list of words contained in these tries is NOT necessarily
;; specified in alphabetic order.

;; blank-trie contains no words.
;; h-u-trie contains: "hot" "hat" "ha" "he" "use"
;; c-d-trie contains: "cat" "catnip" "dog" "donut" "doze"
;;   "catch" "cattle" "cater" "donald" "dogfish" "dig"
;; toydict-trie contains: "actress" "adopt" "awful" 
;;   "billion" "blurt" "cold" "concrete" "cook" "deprive"
;;   "describe" "dirt" "effectively" "fault" "five" "petite"
;;   "publication" "significantly" "smugly" "swapped"
;;   "ugly" "wake"


;; Data definitions needed for the trie question:

(define-struct tnode (key ends-word? children) #:transparent)
;; A TNode is a (make-tnode Char Bool (listof TNode))
;; requires: the TNodes of children are sorted in lexicographical
;;   order by key.

(define-struct trie (children) #:transparent)
;; A Trie is a (make-trie (listof TNode))
;; requires: the TNodes of children are sorted in lexicographical
;;   order by key.

(define blank-trie (make-trie empty))

(define h-u-trie
  (make-trie
   (list
    (make-tnode
     #\h
     false
     (list
      (make-tnode #\a true (list (make-tnode #\t true empty)))
      (make-tnode #\e true empty)
      (make-tnode #\o false (list (make-tnode #\t true empty)))))
    (make-tnode
     #\u
     false
     (list (make-tnode #\s false (list (make-tnode #\e true empty))))))))

(define c-d-trie
  (make-trie
   (list
    (make-tnode
     #\c
     false
     (list
      (make-tnode
       #\a
       false
       (list
        (make-tnode
         #\t
         true
         (list
          (make-tnode #\c false (list (make-tnode #\h true empty)))
          (make-tnode #\e false (list (make-tnode #\r true empty)))
          (make-tnode
           #\n
           false
           (list (make-tnode #\i false (list (make-tnode #\p true empty)))))
          (make-tnode
           #\t
           false
           (list (make-tnode #\l false (list (make-tnode #\e true empty)))))))))))
    (make-tnode
     #\d
     false
     (list
      (make-tnode #\i false (list (make-tnode #\g true empty)))
      (make-tnode
       #\o
       false
       (list
        (make-tnode
         #\g
         true
         (list
          (make-tnode
           #\f
           false
           (list
            (make-tnode
             #\i
             false
             (list (make-tnode #\s false (list (make-tnode #\h true empty)))))))))
        (make-tnode
         #\n
         false
         (list
          (make-tnode
           #\a
           false
           (list (make-tnode #\l false (list (make-tnode #\d true empty)))))
          (make-tnode #\u false (list (make-tnode #\t true empty)))))
        (make-tnode #\z false (list (make-tnode #\e true empty))))))))))

(define toydict-trie
  (make-trie
   (list
    (make-tnode
     #\a
     false
     (list
      (make-tnode
       #\c
       false
       (list
        (make-tnode
         #\t
         false
         (list
          (make-tnode
           #\r
           false
           (list
            (make-tnode
             #\e
             false
             (list (make-tnode #\s false (list (make-tnode #\s true empty)))))))))))
      (make-tnode
       #\d
       false
       (list
        (make-tnode
         #\o
         false
         (list (make-tnode #\p false (list (make-tnode #\t true empty)))))))
      (make-tnode
       #\w
       false
       (list
        (make-tnode
         #\f
         false
         (list (make-tnode #\u false (list (make-tnode #\l true empty)))))))))
    (make-tnode
     #\b
     false
     (list
      (make-tnode
       #\i
       false
       (list
        (make-tnode
         #\l
         false
         (list
          (make-tnode
           #\l
           false
           (list
            (make-tnode
             #\i
             false
             (list (make-tnode #\o false (list (make-tnode #\n true empty)))))))))))
      (make-tnode
       #\l
       false
       (list
        (make-tnode
         #\u
         false
         (list (make-tnode #\r false (list (make-tnode #\t true empty)))))))))
    (make-tnode
     #\c
     false
     (list
      (make-tnode
       #\o
       false
       (list
        (make-tnode #\l false (list (make-tnode #\d true empty)))
        (make-tnode
         #\n
         false
         (list
          (make-tnode
           #\c
           false
           (list
            (make-tnode
             #\r
             false
             (list
              (make-tnode
               #\e
               false
               (list
                (make-tnode #\t false (list (make-tnode #\e true empty)))))))))))
        (make-tnode #\o false (list (make-tnode #\k true empty)))))))
    (make-tnode
     #\d
     false
     (list
      (make-tnode
       #\e
       false
       (list
        (make-tnode
         #\p
         false
         (list
          (make-tnode
           #\r
           false
           (list
            (make-tnode
             #\i
             false
             (list (make-tnode #\v false (list (make-tnode #\e true empty)))))))))
        (make-tnode
         #\s
         false
         (list
          (make-tnode
           #\c
           false
           (list
            (make-tnode
             #\r
             false
             (list
              (make-tnode
               #\i
               false
               (list
                (make-tnode #\b false (list (make-tnode #\e true empty)))))))))))))
      (make-tnode
       #\i
       false
       (list (make-tnode #\r false (list (make-tnode #\t true empty)))))))
    (make-tnode
     #\e
     false
     (list
      (make-tnode
       #\f
       false
       (list
        (make-tnode
         #\f
         false
         (list
          (make-tnode
           #\e
           false
           (list
            (make-tnode
             #\c
             false
             (list
              (make-tnode
               #\t
               false
               (list
                (make-tnode
                 #\i
                 false
                 (list
                  (make-tnode
                   #\v
                   false
                   (list
                    (make-tnode
                     #\e
                     false
                     (list
                      (make-tnode
                       #\l
                       false
                       (list (make-tnode #\y true empty)))))))))))))))))))))
    (make-tnode
     #\f
     false
     (list
      (make-tnode
       #\a
       false
       (list
        (make-tnode
         #\u
         false
         (list (make-tnode #\l false (list (make-tnode #\t true empty)))))))
      (make-tnode
       #\i
       false
       (list (make-tnode #\v false (list (make-tnode #\e true empty)))))))
    (make-tnode
     #\p
     false
     (list
      (make-tnode
       #\e
       false
       (list
        (make-tnode
         #\t
         false
         (list
          (make-tnode
           #\i
           false
           (list (make-tnode #\t false (list (make-tnode #\e true empty)))))))))
      (make-tnode
       #\u
       false
       (list
        (make-tnode
         #\b
         false
         (list
          (make-tnode
           #\l
           false
           (list
            (make-tnode
             #\i
             false
             (list
              (make-tnode
               #\c
               false
               (list
                (make-tnode
                 #\a
                 false
                 (list
                  (make-tnode
                   #\t
                   false
                   (list
                    (make-tnode
                     #\i
                     false
                     (list
                      (make-tnode
                       #\o
                       false
                       (list (make-tnode #\n true empty)))))))))))))))))))))
    (make-tnode
     #\s
     false
     (list
      (make-tnode
       #\i
       false
       (list
        (make-tnode
         #\g
         false
         (list
          (make-tnode
           #\n
           false
           (list
            (make-tnode
             #\i
             false
             (list
              (make-tnode
               #\f
               false
               (list
                (make-tnode
                 #\i
                 false
                 (list
                  (make-tnode
                   #\c
                   false
                   (list
                    (make-tnode
                     #\a
                     false
                     (list
                      (make-tnode
                       #\n
                       false
                       (list
                        (make-tnode
                         #\t
                         false
                         (list
                          (make-tnode
                           #\l
                           false
                           (list (make-tnode #\y true empty)))))))))))))))))))))))
      (make-tnode
       #\m
       false
       (list
        (make-tnode
         #\u
         false
         (list
          (make-tnode
           #\g
           false
           (list (make-tnode #\l false (list (make-tnode #\y true empty)))))))))
      (make-tnode
       #\w
       false
       (list
        (make-tnode
         #\a
         false
         (list
          (make-tnode
           #\p
           false
           (list
            (make-tnode
             #\p
             false
             (list
              (make-tnode #\e false (list (make-tnode #\d true empty)))))))))))))
    (make-tnode
     #\u
     false
     (list
      (make-tnode
       #\g
       false
       (list (make-tnode #\l false (list (make-tnode #\y true empty)))))))
    (make-tnode
     #\w
     false
     (list
      (make-tnode
       #\a
       false
       (list (make-tnode #\k false (list (make-tnode #\e true empty))))))))))
