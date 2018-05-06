#lang racket

;; Do not make any changes to this file!  It provides support for your
;; solution, which should be located in pronunciation.rkt.

;; This is our way of "exporting" the constants toy-dictionary and cmudict
;; so that other programs that use this file have access to it.
(provide cmudict toy-dictionary)

;; A simple dictionary that can be useful for testing.
(define toy-dictionary
  (list
   (list "actress" '((AE 1) K T R (AH 0) S))
   (list "adopt" '((AH 0) D (AA 1) P T))
   (list "awful" '((AA 1) F (AH 0) L))
   (list "billion" '(B (IH 1) L Y (AH 0) N))
   (list "blurt" '(B L (ER 1) T))
   (list "cold" '(K (OW 1) L D))
   (list "concrete" '(K (AH 0) N K R (IY 1) T))
   (list "cook" '(K (UH 1) K))
   (list "deprive" '(D (IH 0) P R (AY 1) V))
   (list "describe" '(D (IH 0) S K R (AY 1) B))
   (list "dirt" '(D (ER 1) T))
   (list "effectively" '((IH 0) F (EH 1) K T (IH 0) V L (IY 0)))
   (list "fault" '(F (AO 1) L T))
   (list "five" '(F (AY 1) V))
   (list "petite" '(P (AH 0) T (IY 1) T))
   (list "publication" '(P (AH 2) B L (IH 0) K (EY 1) SH (AH 0) N))
   (list "significantly" '(S (IH 0) G N (IH 1) F (IH 0) K (AH 0) N T L (IY 0)))
   (list "smugly" '(S M (AH 1) G L (IY 0)))
   (list "swapped" '(S W (AA 1) P T))
   (list "ugly" '((AH 1) G L (IY 0)))
   (list "wake" '(W (EY 1) K))))

;; A constant containing a large (~130,000) number of words for testing
;; and exploration.  Although the dictionary is produced using a big
;; block of code, once it's loaded into Racket it behaves just like the
;; small dictionary above.

(define cmudict
  (local
    [(define (make-phonemes L)
       (map (lambda (s)
              (cond [(regexp-match? #rx"[012]" s)
                     (local
                       [(define l (string-length s))
                        (define snd (substring s 0 (sub1 l)))
                        (define n (substring s (sub1 l) l))]
                       (list (string->symbol snd) (string->number n)))]
                    [else (string->symbol s)])) L))

     (define (parse-line line)
       (local
         [(define L (string-split line))]
         (cond
           [(regexp-match? #rx"^[A-Z]+$" (first L))
            (list (string-downcase (first L)) (make-phonemes (rest L)))]
           [else false])))

     (define (validate-line phons)
       (= 1 (length (filter (lambda (x) (and (cons? x) (= 1 (second x)))) phons))))

     (define (parse-validate-line line)
       (local
         [(define pl (parse-line line))]
         (cond
           [(boolean? pl) false]
           [(validate-line (second pl)) pl]
           [else false])))]
    (filter cons? (map parse-validate-line (file->lines "cmudict.txt")))))

