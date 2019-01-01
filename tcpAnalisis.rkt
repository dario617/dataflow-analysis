#lang racket
(require racket/tcp)
(require "Racket/parser.rkt")
(require "Racket/cfg.rkt")
(require "Racket/dfa.rkt")
(require "Racket/live-var.rkt")
(require "Racket/available-expr.rkt")

;; Hash Hash tcp-in tcp-out -> _
(define (sendHash IN OUT in out)
    (begin
        (displayln (read in))
        (write (string-append (hash->json IN) ";" (hash->json OUT)) out)
        (flush-output out)         
    )
)

;; CFG -> JSON string string
(define (cfg->json cfg)
  (match cfg
    [(Node b l) (string-append "{ \"value\": \"" (~v b) "\" , \"label\":" (~v l) " }")]
    [(Edge (Node b1 l1) (Node b2 l2) l) ( string-append "{ \"l1\": " (~v l1) ", \"l2\": " (~v l2) ",\"l\":" (~v l)"}")]
    [CFG (string-append "{ \"entry\": " (cfg->json (CFG-entry cfg))
                          ", \"exit\":" (cfg->json (CFG-exit cfg))
                          ", \"nodes\": ["
       (foldr (lambda (el acc) (string-append (cfg->json el) (string-append "," acc ))) (cfg->json (car (CFG-nodes cfg))) (cdr (CFG-nodes cfg))) 
                          "], \"edges\": ["
       (foldr (lambda (el acc) (string-append (cfg->json el) (string-append "," acc ))) (cfg->json (car (CFG-edges cfg))) (cdr (CFG-edges cfg)))
       "]}")]
      )
  )
;; Hash -> JSON string string
(define (hash->json h)
  (match h
    [(cons l r)
    (string-append "\"" (~s (Node-label l)) "\": \"" (~v (set->list r)) "\"")]
    [(? hash?) (string-append "{ " (foldr
                                            (lambda (el acc)
                                              (string-append (hash->json el) (string-append "," acc)))
                                            (hash->json (car (hash->list h)))
                                            (cdr (hash->list h))) "}")]
      )
  )

;; Chaotic iteration with output and input
(define (chaotic-iteration-tcp analysis in out)
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
    
    (define (loop IN OUT old-IN old-OUT in out)
      (for ([n (CFG-nodes cfg)])
        (cond [(eq? 'forward direction)
                (let ([preds (map (curry hash-ref OUT) (get-preds n cfg))])
                  (when (not (empty? preds)) (hash-set! IN n (apply meet preds))))
                (hash-set! OUT n (set-union (set-subtract (hash-ref IN n) (kill cfg n)) (gen cfg n)))
                (sendHash IN OUT in out)
                ]
              [(eq? 'backward direction)
                (let ([succs (map (curry hash-ref IN) (get-succs n cfg))])
                  (when (not (empty? succs)) (hash-set! OUT n (apply meet succs))))
                (hash-set! IN n (set-union (set-subtract (hash-ref OUT n) (kill cfg n)) (gen cfg n)))
                (sendHash IN OUT in out)
                ]
              [else (error "not a direction")]))
      (if (and (equal? IN old-IN)(equal? OUT old-OUT))
          (begin
              (sendHash IN OUT in out)
              (cons IN OUT)
              )
          (begin
              ;; Avoid sending empty steps
              (when (eq? 'forward direction)
                (when (not (equal? IN old-IN))
                  (sendHash IN OUT in out)
                )
              )
              (when (eq? 'backward direction)
                (when (not (equal? OUT old-OUT))
                  (sendHash IN OUT in out)
                )
              )
              (loop IN OUT (hash-copy IN) (hash-copy OUT) in out)
            )
          ))
    (loop IN OUT (hash-copy IN) (hash-copy OUT) in out)))

;; Worklist with output and input
(define (worklist-tcp analysis in out)
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

    (define (wlist IN OUT WORKLIST old-IN old-OUT in out)
      (if (empty? (unbox WORKLIST))
        (begin 
          (sendHash IN OUT in out)
          (cons IN OUT)
        )
        (let ([n (car (unbox WORKLIST))])
          (begin
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
            (begin
              ;; Avoid sending empty steps
              (when (eq? 'backward direction)
                (when (not (equal? OUT old-OUT))
                  (sendHash IN OUT in out)
                )
              )
              (when (eq? 'forward direction)
                (when (not (equal? IN old-IN))
                  (sendHash IN OUT in out)
                )
              )
              (wlist IN OUT WORKLIST (hash-copy IN) (hash-copy OUT) in out)
            )
          )
        )
        )
      )
    (wlist IN OUT WORKLIST (hash-copy IN) (hash-copy OUT) in out)
  )
)

;; TCP server to provide an interface for dataVisualizer
(define (tcpServer)
  (define the-listener (tcp-listen 9876))
  (define-values (in out) (tcp-accept the-listener))
  (define (readLoop in out mode anl exitSet)
    (define inputRcv (read in))
    (write '{RKT: copy that\n} out)
    (flush-output out)
    (match inputRcv
      ['getGraph (begin 
        (printf "RKT console: Doing CFG\n")
        (write (cfg->json (stmt->cfg (parse-stmt (read in)))) out)
        (flush-output out)
        (readLoop in out mode anl exitSet)
        )]
      ['chaotic (begin
        (printf "RKT console: Chaotic Its On\n")
        (write '{RKT: copy that Chaotic\n} out)
        (flush-output out)
        (readLoop in out 0 anl exitSet)
        )]
      ['worklist (begin
        (printf "RKT console: Worklist Its On\n")
        (write '{RKT: copy that Worklist\n} out)
        (flush-output out)
        (readLoop in out 1 anl exitSet)
        )]
      ['liveVar (begin
        (printf "RKT console: Live Var Its On\n")
        (write '{RKT: copy that LiveVars\n} out)
        (flush-output out)
        (readLoop in out mode 0 exitSet)
        )]
      ['avExpr (begin
        (printf "RKT console: Available Expr Its On!\n")
        (write '{RKT: copy that Available Expr\n} out)
        (flush-output out)
        (readLoop in out mode 1 exitSet)
        )]
      ['doAnalysis (begin
        (printf "RKT console: Analysis incoming!\n")
        (define Dee-stmt (read in))
        (write '{RKT: got the stmt\n} out)
        (flush-output out)
        (cond
          [(equal? mode 0)
            (cond 
              [(equal? anl 0)
                ((chaotic-iteration-tcp (live-variables-analysis exitSet) in out) (parse-stmt Dee-stmt))]
              [(equal? anl 1)
                ((chaotic-iteration-tcp avail-expr-analysis in out) (parse-stmt Dee-stmt))]
          )]
          [(equal? mode 1)
            (cond 
              [(equal? anl 0)
                ((worklist-tcp (live-variables-analysis exitSet) in out) (parse-stmt Dee-stmt))]
              [(equal? anl 1)
                ((worklist-tcp avail-expr-analysis in out) (parse-stmt Dee-stmt))]
          )]
        )
        (printf "RKT console: Done\n")
        (define ack-input (read in))
        (write "       RKT: Done\n" out)
        (flush-output out)
        (readLoop in out mode anl exitSet))]
      [`{,stmts ...} (begin
        (printf "RKT console: Got exit variables\n")
        (write '{RKT: copy that variables at exit\n} out)
        (flush-output out)
        (readLoop in out mode anl (list->set inputRcv))
        )
      ]
      [ else (begin (printf "~v" inputRcv)(tcp-close the-listener)(tcp-abandon-port in)(tcp-abandon-port out))]
    )
  )
  (readLoop in out 1 0 (set))
)

(printf "Starting racket analysis server\n")
;( serve 9876 )
(letrec ([securityLoop (lambda () (begin (tcpServer)(securityLoop) ))])
  (securityLoop)
)