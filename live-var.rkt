#lang racket

;; Live Variables Analysis
;; A variable is live at a program point if its
;; current value may be read during the remaining
;; execution of program.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)


(provide (all-defined-out))

;; Set -> Analysis
(define live-variables-analysis (λ ([initialExitSet null])
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg n) (set))
   ; entry fact
   (λ (fun cfg entry) (set))
   ; exit fact
   (λ (fun cfg exit) (if (null? initialExitSet) (set) initialExitSet))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (get-vars e)]
       [(Node (Equal l r) _)
        (set-union (get-vars l) (get-vars r))]
       [(Node (Greater l r) _)
        (set-union (get-vars l) (get-vars r))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (set id)]
       [else (set)]))
   ; meet
   set-union))
)

;;;;;;;;;;;;;;;;;;;;;;; Functions for testing the method ;;;;;;;;;;;;;;;;;;;;;;;
(define live-variables-star (λ (stmt initialVars)
  ((chaotic-iteration (live-variables-analysis initialVars) ) stmt))
)

(define live-variables-star-worklist (λ (stmt initialVars)
  ((worklist (live-variables-analysis initialVars) ) stmt))
)

(define live-variables
  (chaotic-iteration (live-variables-analysis) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TEST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
(define test-stmt
  (parse-stmt
   '{{:= x 2}
     {:= y 4}
     {:= x 1}
     {if {> y x}
         {:= z y}
         {:= z {* y y}}}
     {:= x z}}))

(define result (live-variables test-stmt))
(define result-OUT (cdr result))

(check-equal? (make-immutable-hash (hash->list result-OUT))
              (hash
               (Node (NoOp) 7) (set 'z)
               (Node (Greater 'y 'x) 6) (set 'y)
               (Node (Assign 'x 1) 3) (set 'x 'y)
               (Node (Assign 'y 4) 2) (set 'y)
               (Node (Assign 'z 'y) 4) (set 'z)
               (Node (Assign 'x 2) 1) (set)
               (Node (Assign 'x 'z) 8) (set)
               (Node (Assign 'z (Mult 'y 'y)) 5) (set 'z)))
  
(check-equal? (make-immutable-hash (hash->list (cdr (live-variables-star test-stmt (set 'x 'y 'z)))))
              (hash
               (Node (NoOp) 7) (set 'z 'y)
               (Node (Greater 'y 'x) 6) (set 'y)
               (Node (Assign 'x 1) 3) (set 'x 'y)
               (Node (Assign 'y 4) 2) (set 'y)
               (Node (Assign 'z 'y) 4) (set 'z 'y)
               (Node (Assign 'x 2) 1) (set)
               (Node (Assign 'x 'z) 8) (set 'x 'y 'z)
               (Node (Assign 'z (Mult 'y 'y)) 5) (set 'z 'y)))

;;;;;;;;;;;;;;;;;;;;;;; Test with worklist ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(define result-OUT-worklist (cdr (live-variables-star-worklist test-stmt (set 'x 'y 'z))))
  
(check-equal? (make-immutable-hash (hash->list result-OUT-worklist))
              (hash
               (Node (NoOp) 7) (set 'z 'y)
               (Node (Greater 'y 'x) 6) (set 'y)
               (Node (Assign 'x 1) 3) (set 'x 'y)
               (Node (Assign 'y 4) 2) (set 'y)
               (Node (Assign 'z 'y) 4) (set 'z 'y)
               (Node (Assign 'x 2) 1) (set)
               (Node (Assign 'x 'z) 8) (set 'x 'y 'z)
               (Node (Assign 'z (Mult 'y 'y)) 5) (set 'z 'y)))
  
  )