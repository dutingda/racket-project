#lang racket

;; The Cameli Algorithm

;; Prepend elem to every element in l
(define (prae elem l)
  (map (lambda (x) (cons elem x)) l)
)
;; The Scala for-yield loop, concatenating
;;
;; Takes all outputs of func in the range [iMin,iMax] and concatenates them together.
(define (for-yield-append iMin iMax func)
  (if (> iMin iMax)
      empty
      ((lambda (x)
         (if (empty? x)
             (for-yield-append (add1 iMin) iMax func)
             (append x (for-yield-append (add1 iMin) iMax func))
         )
      ) (func iMin))
  )
)

(define (exact-floor x)
  (inexact->exact (floor x))
)
(define (exact-ceil x)
  (inexact->exact (ceiling x))
)

;; Bounds for v0
(define v0-min 2)
(define (v0-max k r) (+ k r))

;; Bounds for vi
(define (vi-max r l L S div)
  (define rL (/ r L))
  (cond
    [(>= S 1) 0]
    [(>= (+ S rL) 1)
     ((lambda (c)
        (vi-max r l (* c L) S div)
     )
      (add1 (exact-floor (/ rL (- 1 S))))
     )
    ]
    [true
     (exact-floor (/ l (- 1 (+ S rL))))
    ]
  )
)

;; All these functions are required to return list of lists.

;; Final stage (v now fixed) reached
(define (final r L S)
  ;;(printf "Final reached: ~a ~a" L S) (newline)
  (define (inner-final n)
    (if (and (integer? n) (= 0 (remainder n L)))
        (list (list n))
        empty
    )
  )
  (if (>= S 1)
      empty
      (inner-final (/ r (- 1 S)))
  )
)
;; Have not reached final stage yet. Still l degrees of freedom left.
(define (infinal r l L S vJ-1)
  (define minI vJ-1)
  (define maxI (vi-max r l vJ-1 S vJ-1))
  (define (depth-it vI) ;; Iterate through possible values of vI
    (prae vI (depth
     r ; Next r is previous r
     (sub1 l) ;; Next l is previous l - 1
     (lcm vI L) ;; Next L is lcm of previous L with vI
     (+ (/ 1 vI) S) ;; next S is sum of previous S with 1/vI
     vI
    ))
  )
  (for-yield-append minI maxI depth-it)
)
;;
;; It is not this routine's responsibility to prepend vJ-1 in front of its outputs.
(define (depth r l L S vJ-1)
  (cond
    [(>= S 1) empty]
    [(= 0 l) (final r L S)]
    [true (infinal r l L S vJ-1)]
  )
)
(define (main k r)
  (define min0 v0-min)
  (define max0 (v0-max k r))
  (define (main-it v0)
    (prae v0 (depth r (sub1 k) v0 (/ 1 v0) v0))
  )
  (for-yield-append min0 max0 main-it)
)

(define out (main 5 2))
out
(length out)