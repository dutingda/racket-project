;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname omit) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; the foldr combine the result and the newly generated list of list of characters
;; which is the the first character was mapping to each of the already existed results
;; and glued together into a new list of list of characters and repeat the step it will
;; grow larger to get all of its substring
(define (omit2 s)
  (map list->string (foldr (lambda (fc result) 
      (append (map (lambda (a) (cons fc a)) result) result)) (list'()) (string->list s))))

(omit2 "abcdefghijklmnopqrstu")

;; Cofined in some statements in omit2, it must satisfy the condition of omit1
(define (omit1 s)
  (omit2 s))

#|we use a fixed-point combinator (not sure why we are expected to know this)
called Y Combinator. We first write map and append with lambda only.
(define mapp
  ((lambda (r) (r r))
   (lambda (fcn)(lambda (f x)
     (cond[(empty? x) empty]
     [else (cons (f (first x)) ((fcn fcn) f (rest x)))])))))

(define appen ((lambda (r) (r r))
               (lambda (fcn) (lambda (x y)
               (cond[(and (empty? x) (empty? y)) empty]
                    [(empty? x) y]
                    [else (cons (first x) ((fcn fcn) (rest x) y))])))))
Then we can proceed to write the whole program in lambda|#

(define (omit3 s)
  (((lambda (r) (r r)) (lambda (fcn)
                         (lambda (f x)
                           (cond[(empty? x) empty]
                                [else (cons (f (first x))
                                            ((fcn fcn) f (rest x)))]))))
 list->string
 ((lambda (f x) (f f x))
 (lambda (om s)
 (cond [(empty? s) (list '())]
       [else (((lambda (r) (r r))
               (lambda (fcn)
                 (lambda (x y)
                 (cond[(and (empty? x) (empty? y)) empty]
                      [(empty? x) y]
                      [else (cons (first x) ((fcn fcn) (rest x) y))]))))
                   (((lambda (r) (r r))
                     (lambda (fcn)(lambda (f x)
                                  (cond[(empty? x) empty]
                                       [else (cons (f (first x))
                                                   ((fcn fcn) f (rest x)))]))))
                    (lambda (x) (cons (first s) x))
                    (om om (rest s)))
                 (om om (rest s)))] )) (string->list s))))









