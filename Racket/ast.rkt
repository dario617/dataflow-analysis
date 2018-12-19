#lang racket

;; AST of TIP language

(provide (all-defined-out))

(struct Fun (name args locals body) #:transparent)

#|
Var ::= Symbol
Expr ::= Int | Bool | Var | Plus | Minus | Mult | Div | Greater | Equal
|#

(struct Plus (lhs rhs) #:transparent)
(struct Minus (lhs rhs) #:transparent)
(struct Mult (lhs rhs) #:transparent)
(struct Div (lhs rhs) #:transparent)
(struct Greater (lhs rhs) #:transparent)
(struct Equal (lhs rhs) #:transparent)

#|
Stmt ::= NoOp | While | Assign | If | Stmt*
|#

(struct NoOp () #:transparent)
(struct While (cnd body) #:transparent)
(struct Assign (id e) #:transparent)
(struct If (cnd thn els) #:transparent)
(struct Seq (stmts) #:transparent)

;; Auxiliary functions

(define not-constant?
  (λ (x) (and (not (integer? x)) (not (boolean? x)))))

(define (expr-contains-var? expr var)
  (match expr
    [(? integer?) #f]
    [(? boolean? x) #f]
    [(? symbol? x) (eq? x var)]
    [(Plus l r) (or (expr-contains-var? l var)
                    (expr-contains-var? r var))]
    [(Minus l r) (or (expr-contains-var? l var)
                     (expr-contains-var? r var))]
    [(Mult l r) (or (expr-contains-var? l var)
                    (expr-contains-var? r var))]
    [(Div l r) (or (expr-contains-var? l var)
                   (expr-contains-var? r var))]
    [(Greater l r) (or (expr-contains-var? l var)
                       (expr-contains-var? r var))]
    [(Equal l r) (or (expr-contains-var? l var)
                     (expr-contains-var? r var))]
    [else #f]))

(define (get-vars e)
  (match e
    [(? symbol? x) (set x)]
    [(Plus l r) (set-union (get-vars l) (get-vars r))]
    [(Minus l r) (set-union (get-vars l) (get-vars r))]
    [(Mult l r) (set-union (get-vars l) (get-vars r))]
    [(Div l r) (set-union (get-vars l) (get-vars r))]
    [(Greater l r) (set-union (get-vars l) (get-vars r))]
    [(Equal l r) (set-union (get-vars l) (get-vars r))]
    [else (set)]))