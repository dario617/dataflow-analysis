#lang racket

;; Very Busy Expression Analysis
;; An expression is very busy if, no matter what
;; path is taken, it will definitely be evaluated
;; again before its value changes.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define very-busy-analysis
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg node) (list->set (filter not-constant? (get-exprs cfg))))
   ; entry fact
   null
   ; exit fact
   (λ (fun cfg n) (set))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (if (not-constant? e) (set e) (set))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (list->set (filter (λ (e1) (expr-contains-var? e1 id))
                           (set->list (get-exprs cfg))))]
       [else (set)]))
   ; meet
   set-intersect
   ))

(define very-busy-exprs
  (chaotic-iteration very-busy-analysis))

(module+ test
  (define test-stmt
    (parse-stmt '{{if {== a b}
                    {{:= x {- b a}}
                    {:= y {- a b}}}
                  {{:= y {- b a}}
                    {:= a 0}
                    {:= x {- a b}}}}}
              ))

  (define result (very-busy-exprs test-stmt))
  (define result-IN (car result))
  (define result-OUT (cdr result))
  (check-equal? (make-immutable-hash (hash->list result-IN))
                (hash
                 (Node (Assign 'y (Minus 'a 'b)) 2)
                 (set (Minus 'a 'b))
                 (Node (Assign 'x (Minus 'b 'a)) 1)
                 (set (Minus 'a 'b) (Minus 'b 'a))
                 (Node (Assign 'a 0) 4)
                 (set)
                 (Node (Assign 'y (Minus 'b 'a)) 3)
                 (set (Minus 'b 'a))
                 (Node (Equal 'a 'b) 6)
                 (set (Minus 'b 'a))
                 (Node (NoOp) 7)
                 (set)
                 (Node (Assign 'x (Minus 'a 'b)) 5)
                 (set (Minus 'a 'b))))

  (check-equal? (make-immutable-hash (hash->list result-OUT))
                (hash
                 (Node (Assign 'y (Minus 'a 'b)) 2)
                 (set)
                 (Node (Assign 'x (Minus 'b 'a)) 1)
                 (set (Minus 'a 'b))
                 (Node (Assign 'a 0) 4)
                 (set (Minus 'a 'b))
                 (Node (Assign 'y (Minus 'b 'a)) 3)
                 (set)
                 (Node (Equal 'a 'b) 6)
                 (set (Minus 'b 'a))
                 (Node (NoOp) 7)
                 (set)
                 (Node (Assign 'x (Minus 'a 'b)) 5)
                 (set))))