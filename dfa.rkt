#lang racket

;; The framework of dataflow analysis
;; Chaotic Iteration Algorithm

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")

(provide (all-defined-out))

(struct Analysis (direction
                  init
                  entry-fact
                  exit-fact
                  gen
                  kill
                  meet))

(define (chaotic-iteration analysis)
  (define init (Analysis-init analysis))
  (define direction (Analysis-direction analysis))
  (define entry-fact (Analysis-entry-fact analysis))
  (define exit-fact (Analysis-exit-fact analysis))
  (define gen (Analysis-gen analysis))
  (define kill (Analysis-kill analysis))
  (define meet (Analysis-meet analysis))

  (lambda (fun)
    (define cfg (stmt->cfg fun))
    (define IN (make-hash))
    (define OUT (make-hash))
    
    (for ([n (CFG-nodes cfg)])
        (hash-set! IN n (init cfg n))
        (hash-set! OUT n (init cfg n)))

    (when (not (empty? entry-fact))
      (hash-set! IN (CFG-entry cfg) (entry-fact fun cfg (CFG-entry cfg))))
    (when (not (empty? exit-fact))
      (hash-set! OUT (CFG-exit cfg) (exit-fact fun cfg (CFG-exit cfg))))

    (hash-set! IN (CFG-exit cfg) (set))
    (hash-set! OUT (CFG-entry cfg) (set))
    
    (define (loop IN OUT old-IN old-OUT)
      (for ([n (CFG-nodes cfg)])
        (cond [(eq? 'forward direction)
               (let ([preds (map (curry hash-ref OUT) (get-preds n cfg))])
                 (when (not (empty? preds)) (hash-set! IN n (apply meet preds))))
               (hash-set! OUT n (set-union (set-subtract (hash-ref IN n) (kill cfg n)) (gen cfg n)))]
              [(eq? 'backward direction)
               (let ([succs (map (curry hash-ref IN) (get-succs n cfg))])
                 (when (not (empty? succs)) (hash-set! OUT n (apply meet succs))))
               (hash-set! IN n (set-union (set-subtract (hash-ref OUT n) (kill cfg n)) (gen cfg n)))]
              [else (error "not a direction")]))
      
      (if (and (equal? IN old-IN)
               (equal? OUT old-OUT))
          (cons IN OUT)
          (loop IN OUT (hash-copy IN) (hash-copy OUT))))
    (loop IN OUT (hash-copy IN) (hash-copy OUT))))

(define (worklist analysis)
  (define init (Analysis-init analysis))
  (define direction (Analysis-direction analysis))
  (define entry-fact (Analysis-entry-fact analysis))
  (define exit-fact (Analysis-exit-fact analysis))
  (define gen (Analysis-gen analysis))
  (define kill (Analysis-kill analysis))
  (define meet (Analysis-meet analysis))

  (lambda (fun)
    (define cfg (stmt->cfg fun))
    (define IN (make-hash))
    (define OUT (make-hash))
    (define WORKLIST (box (list)))
    
    (for ([n (CFG-nodes cfg)])
        (hash-set! IN n (init cfg n))
        (hash-set! OUT n (init cfg n))
        (set-box! WORKLIST (append (unbox WORKLIST) (list n))) )

    (when (not (empty? entry-fact))
      (hash-set! IN (CFG-entry cfg) (entry-fact fun cfg (CFG-entry cfg))))
    (when (not (empty? exit-fact))
      (hash-set! OUT (CFG-exit cfg) (exit-fact fun cfg (CFG-exit cfg))))

    (hash-set! IN (CFG-exit cfg) (set))
    (hash-set! OUT (CFG-entry cfg) (set))

    (define (wlist IN OUT WORKLIST old-IN old-OUT)
      (if (empty? (unbox WORKLIST))
        (cons IN OUT)
        (let ([n (car (unbox WORKLIST))])
          (begin
            (printf "Did iter ~v \n" (unbox WORKLIST))
            ;; Remove n from worklist
            (set-box! WORKLIST (remove n (unbox WORKLIST)))
            ;; Compute the stuff
            (cond
              [(eq? 'forward direction)
               (let ([preds (map (curry hash-ref OUT) (get-preds n cfg))])
                 (when (not (empty? preds)) (hash-set! IN n (apply meet preds))))
               (hash-set! OUT n (set-union (set-subtract (hash-ref IN n) (kill cfg n)) (gen cfg n)))
               ;; If the value is different from the previous one
               (when (not (equal? OUT old-OUT)) (set-box! WORKLIST (append (unbox WORKLIST) (get-succs n cfg) )) )
                ]
              [(eq? 'backward direction)
               (let ([succs (map (curry hash-ref IN) (get-succs n cfg))])
                 (when (not (empty? succs)) (hash-set! OUT n (apply meet succs))))
               (hash-set! IN n (set-union (set-subtract (hash-ref OUT n) (kill cfg n)) (gen cfg n)))
               ;; If the value is different from the previous one
               (when (not (equal? IN old-IN)) (set-box! WORKLIST (append (unbox WORKLIST) (get-preds n cfg) )) )
               ]
              [else (error "not a direction")]
              )
            (wlist IN OUT WORKLIST (hash-copy IN) (hash-copy OUT))
          )
        )
        )
      )
    (wlist IN OUT WORKLIST (hash-copy IN) (hash-copy OUT)))
  )