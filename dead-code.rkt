#lang racket
;; Dead Code remover

(require "live-var.rkt")
(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")

(require rackunit)

;; Live: Stmt Set -> Set
(define (live stmt initialVars)
  (define parsed (parse-stmt stmt))
  (define cfg (stmt->cfg parsed))
  (define live-vari (live-variables-star parsed initialVars))
  (if (eq? (length (CFG-nodes cfg)) 1) initialVars
  ;; Give me the set! of both things
  ;;(set-union (hash-ref (car live-vari) (CFG-entry cfg)) (hash-ref (cdr live-vari) (CFG-entry cfg)))
      (hash-ref (car live-vari) (CFG-entry cfg))
      )
 )

;; Bury: expr Set -> Stmt
;; NB: No Seq of size zero are allowed
(define (bury stmt O)
  (define (helper stmt O seq)
    (match stmt
      [`{noop} (NoOp)]
      [`{:= ,id ,e} ;; Only this id is important for the analysis!
       (if (set-member? O id) (Assign (parse-expr id)  (parse-expr e)) (NoOp))]
      [`{if ,cnd ,thn ,els} (If (parse-expr cnd)
                                (helper thn O #f)
                                (helper els O #f))]
      [`{while ,cnd ,body} (While (parse-expr cnd)
                                  ;; This step was changed during testing
                                  (helper body O #f))]
      [`{,stmt1, stmt2}
       ;; bury first part with the OUT of the rest
       (if seq
       (cons (helper stmt1 (live stmt2 O) #t)
             (cons (helper stmt2 O #t) (list)))
       (Seq (helper stmt O #t)))]
      [`{,stmts ...}
       ;; bury first part with the OUT of the rest
       (if seq
       (cons (helper (car stmt) (live (cdr stmt) O) #t)
             (helper (cdr stmt) O #t) )
       (Seq (helper stmt O #t)))]
      [else (error 'parse-stmt "error in bury while parsing")]
      )
    )
  (helper stmt O #f)
)

(module+ test
;; Test Seq and If
(define test-stmt1'{{:= x 2}
     {:= y 4}
     {:= x 1}
     {if {> y x}
         {:= z y}
         {:= z {* y y}}}
     {:= x z}})

;; Test Seq and While
(define test-stmt2'{{:= x 2}
     {:= y 4}
     {:= x 1}
     {while {> y x}
         {:= z {* y y}}}
     {:= x z}})

;; Test Seq within Seq
(define test-stmt3'{{:= x 2}
     {:= y 4}
     {:= x 1}
     {:= x 2}
     {:= x 3}
     {while {> y x}{       
         {:= x 1}
         {:= z {* y y}}}}
     {:= x z}})
  
(define test-stmt4
   '{{:= x 2}
     {:= y 4}
     {:= x 1}
     {if {> y x}
         {:= x 1}
         {:= z {* y y}}}
     {:= x 2}
     {:= x 3}
     {if {> y x} 
         {:= x 1}
         {:= z {* y y}}}
     {:= x z}})

  
(define result1 (bury test-stmt1 (set 'x 'y 'z)))
  
(define result2 (bury test-stmt2 (set 'x 'y 'z)))
  
(define result3 (bury test-stmt3 (set 'x 'y 'z)))

(define result4 (bury test-stmt4 (set 'x 'y 'z)))
  
(check-equal? result1
              (Seq
               (list
                (NoOp)
                (Assign 'y 4)
                (Assign 'x 1)
                (If (Greater 'y 'x) (Assign 'z 'y) (Assign 'z (Mult 'y 'y)))
                (Assign 'x 'z))))

(check-equal? result2
              (Seq
               (list
                (NoOp)
                (Assign 'y 4)
                (Assign 'x 1)
                (While (Greater 'y 'x) (Assign 'z (Mult 'y 'y)))
                (Assign 'x 'z))))
  
(check-equal? result3
              (Seq
               (list
                (NoOp)
                (Assign 'y 4)
                (NoOp)
                (NoOp)
                (Assign 'x 3)
                (While (Greater 'y 'x) (
                  Seq (list (Assign 'x 1) (Assign 'z (Mult 'y 'y)))))
                (Assign 'x 'z))))

  
(check-equal? result4
              (Seq
               (list
                (NoOp)
                (Assign 'y 4)
                (Assign 'x 1)
                (If (Greater 'y 'x) (NoOp) (Assign 'z (Mult 'y 'y)))
                (NoOp)
                (Assign 'x 3)
                (If (Greater 'y 'x) (Assign 'x '1) (Assign 'z (Mult 'y 'y)))
                (Assign 'x 'z))))
  
  )