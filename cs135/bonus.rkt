;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bonus) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))



(define (remove-word-h loc lotn)
  (cond
    [(empty? lotn) empty]
    [(empty? loc) acc]
    [(char=? (first loc) (tnode-key (first lotnode)))
     (append (remove-word-h (rest loc) (t-node lotn)) (rest lotn))]
    [(char<? (first loc) (tnode-key (first lotnode))
     
    





(define (remove-word str trie)
)
  