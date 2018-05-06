(module a09 (lib "plt-pretty-big-text.ss" "lang")
  
  ;; DO NOT MODIFY THIS FILE!  It contains routines provided by us, and
  ;; must operate exactly as given.  If you make changes, and then submit
  ;; code that relies on those changes, there is no guarantee that your
  ;; code will work at all with our tests.
 
  ;; (search at-end? neighbours a-state) searches a space of states for a
  ;;   goal state, given a predicate at-end? to determine if a given state
  ;;   is a goal state, a function neighbours that produces all next steps
  ;;   from a given state, and a starting state a-state.
  ;; search: (X -> Bool) (X -> (listof X)) X -> (anyof X false)
  
  (define (search at-end? neighbours a-state)
    (local
      [;; (find-route a-state) searches outward from a-state to see if
       ;; there's a path to a solution.
       ;; find-route: X -> (anyof X false)
       (define (find-route a-state)
           (cond
             [(at-end? a-state) a-state]
             [else (find-route/list (neighbours a-state))]))
       
       ;; (find-route/list lostate) searches outward from every state in
       ;; lostate.  If any one of them leads to a solution, stop and
       ;; produce that solution.  Produce false if you run out of options.
       ;; find-route/list: (listof X) -> (anyof X false)
       (define (find-route/list lostate)
         (cond
           [(empty? lostate) false]
           [else
            (local
              [(define cur (find-route (first lostate)))]
              (cond
                [(not (boolean? cur)) cur]
                [else (find-route/list (rest lostate))]))]))]
      (find-route a-state)))
  
  ;; (lists-equiv? l1 l2) consumes two lists l1 and l2, and determines whether
  ;; the lists are essentially the same up to reordering.  This function will
  ;; work as long as one of the two lists contains no duplicates.  Not really
  ;; part of this assignment, but generally useful in tests where we don't care
  ;; about ordering.
  ;; lists-equiv?: (listof X) (listof X) -> Bool
  ;; Examples:
  ;; (check-expect (lists-equiv? '("1" "2" "3") '("2" "3" "1")) true)
  ;; (check-expect (lists-equiv? '(1 2 3 4) '(2 3 4 5)) false)
  
  (define (lists-equiv? l1 l2)
    ;; The approach is a bit sneaky, but very succinct.  Check that
    ;; every element of l1 appears somewhere in l2 (in terms of equal?),
    ;; and that every elements of l2 appears somewhere in l1.
    (and (= (length l1) (length l2))
         (andmap (lambda (x1) (ormap (lambda (x2) (equal? x1 x2)) l2)) l1)
         (andmap (lambda (x2) (ormap (lambda (x1) (equal? x1 x2)) l1)) l2)))
  
  (provide search 
           lists-equiv?))
