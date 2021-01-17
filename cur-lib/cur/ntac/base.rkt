#lang s-exp "../main.rkt"
;; Proof tree representation and top-level syntax

(require
  "../stdlib/sugar.rkt"
  (only-in racket [define r:define])
  (for-syntax "ctx.rkt"
              macrotypes/stx-utils
              racket/match racket/list racket/pretty))

(provide
 define-theorem
 define-theorem/for-export
 ntac
 ntac/debug)

(begin-for-syntax
  (provide
   ntac-syntax
   current-tracing?
   proof-step-inspector

   qed next

   (struct-out ntt)
   (struct-out ntt-hole)
   make-ntt-hole
   (struct-out ntt-exact)
   make-ntt-exact
   (struct-out ntt-context)
   make-ntt-context
   (struct-out ntt-apply)
   make-ntt-apply
   (struct-out ntt-done)
   make-ntt-done

   ;; proof tree zipper
   (struct-out nttz)
   make-nttz nttz-up nttz-down-context nttz-down-apply nttz-down-done nttz-done?

   num-holes
   num-holes/z
   num-holes/z/local
   to-top

   new-proof-tree
   proof-tree->complete-term
   eval-proof-script
   eval-proof-step
   eval-proof-steps
   ntac-proc)

  ;; NTac proof Tree
  (struct ntt (contains-hole? goal) #:transparent)

  ;; This is gross boilerplate to obtain default fields.
  (struct ntt-hole ntt () #:transparent #:constructor-name _ntt-hole)
  (define (make-ntt-hole goal)
    (_ntt-hole #t goal))

  (struct ntt-exact ntt (term) #:transparent #:constructor-name _ntt-exact)
  (define (make-ntt-exact goal term)
    (_ntt-exact #f goal term))

  (struct ntt-context ntt (env-transformer subtree) #:transparent #:constructor-name _ntt-context)
  (define (make-ntt-context f k)
    (_ntt-context (ntt-contains-hole? k) (ntt-goal k) f k))

  (struct ntt-apply ntt (subterms tactic) #:transparent #:constructor-name _ntt-apply)
  (define (make-ntt-apply goal subterms tactic)
    (_ntt-apply (ormap ntt-contains-hole? subterms) goal subterms tactic))

  (struct ntt-done ntt (subtree) #:transparent #:constructor-name _ntt-done)
  (define (make-ntt-done subtree)
    (when (and (not (current-tracing?)) (ntt-contains-hole? subtree))
      (error 'ntt-done "Cannot construct done if hole present: ~v" subtree))
    (_ntt-done #f (ntt-goal subtree) subtree))

  ;; Add the ability to tag ntts with extra data
  (struct ntt-hole-tag ntt-hole [tag] #:transparent #:constructor-name _ntt-hole-tag)
  (define (make-ntt-hole-tag hole tag)
    (match-define (ntt-hole contains-hole? goal) hole)
    (_ntt-hole-tag contains-hole? goal tag))

  (struct ntt-exact-tag ntt-exact [tag] #:transparent #:constructor-name _ntt-exact-tag)
  (define (make-ntt-exact-tag hole tag)
    (match-define (ntt-exact contains-hole? goal term) hole)
    (_ntt-exact-tag contains-hole? goal term tag))
  
  (struct ntt-context-tag ntt-context [tag] #:transparent #:constructor-name _ntt-context-tag)
  (define (make-ntt-context-tag hole tag)
    (match-define (ntt-context contains-hole? goal env-transformer subtree) hole)
    (_ntt-context-tag contains-hole? goal env-transformer subtree tag))
  
  (struct ntt-apply-tag ntt-apply [tag] #:transparent #:constructor-name _ntt-apply-tag)
  (define (make-ntt-apply-tag hole tag)
    (match-define (ntt-apply contains-hole? goal subterms tactic) hole)
    (_ntt-apply-tag contains-hole? goal subterms tactic tag))

  (struct ntt-done-tag ntt-done [tag] #:transparent #:constructor-name _ntt-done-tag)
  (define (make-ntt-done-tag hole tag)
    (match-define (ntt-done contains-hole? goal subtree) hole)
    (_ntt-done-tag contains-hole? goal subtree tag))

  (define (is-tagged-ntt? ntt)
    (or (ntt-hole-tag? ntt)
        (ntt-exact-tag? ntt)
        (ntt-context-tag? ntt)
        (ntt-apply-tag? ntt)
        (ntt-done-tag? ntt)))

  (define (set-tag ntt tag)
    (match ntt
      [(ntt-hole _ _) (make-ntt-hole-tag ntt tag)]
      [(ntt-exact _ _ _) (make-ntt-exact-tag ntt tag)]
      [(ntt-context _ _ _ _) (make-ntt-context-tag ntt tag)]
      [(ntt-apply _ _ _ _) (make-ntt-apply-tag ntt tag)]
      [(ntt-done _ _ _) (make-ntt-done-tag ntt tag)]))

  ;; Assumes ntt is a tagged ntt, fails otherwise
  (define (get-tag ntt tag)
    (match ntt
      [(ntt-hole-tag _ _ tag) tag]
      [(ntt-exact-tag _ _ _ tag) tag]
      [(ntt-context-tag _ _ _ _ tag) tag]
      [(ntt-apply-tag _ _ _ _ tag) tag]
      [(ntt-done-tag _ _ _ tag) tag]))

  (define (copy-tag-if-present source dest)
    (if (is-tagged-ntt? source)
        (set-tag dest (get-tag source))
        dest))

  (define (new-proof-tree goal)
    (make-ntt-hole goal))

  (require racket/trace)
  (define (proof-tree->complete-term pt [err-stx #f])
    (let loop ([pt pt])
      (match pt
        [(ntt-hole _ _)
         (raise-syntax-error 'define-theorem "attempt to save incomplete proof" err-stx)]
        [(ntt-exact _ _ a) a]
        [(ntt-context _ _ gf k)
         (loop k)]
        [(ntt-apply _ _ cs f)
         (apply f (map (λ (c) (loop c)) cs))]
        [(ntt-done _ _ k)
         (loop k)])))
  (define (num-holes/z ptz) (num-holes/z/local (to-top ptz)))
  (define (num-holes/z/local ptz) (num-holes (nttz-focus ptz)))
  (define (num-holes pt)
    (match pt
      [(ntt-hole _ _) 1]
      [(ntt-exact _ _ _) 0]
      [(ntt-context _ _ _ k) (num-holes k)]
      [(ntt-apply _ _ cs f) (apply + (map num-holes cs))]
      [(ntt-done _ _ k) (num-holes k)]))

  ;; NTac proof Tree Zipper
  ;; TODO: track number of holes/subgoals?
  (struct nttz (context focus prev) #:constructor-name _nttz)
  ;; context : NtacCtx (see ctx.rkt)
  ;; focus   : ntt
  ;; prev    : ntt -> nttz
  ;; Produces a new zipper from the current focus

  (define (make-nttz pt [ctxt (mk-empty-ctx)])
    (_nttz ctxt pt
           (λ (last-pt)
             (make-nttz (make-ntt-done last-pt)))))

  (define (to-top tz)
    (if (nttz-done? tz)
        tz
        (parameterize ([current-tracing? #t]) ; TODO: hack to avoid ntt-done err; replace with new param
          (to-top (nttz-up tz)))))
  (define (nttz-up nttz)
    ((nttz-prev nttz) (nttz-focus nttz)))

  (define (nttz-down-done tz)
    (match-define (nttz context foc up) tz)
    (match-define (ntt-done _ _ k) foc)
    (_nttz context k (λ (new-k) (_nttz context (copy-tag-if-present foc (make-ntt-done new-k)) up))))

  (define (nttz-down-context tz)
    (match-define (nttz context foc up) tz)
    (match-define (ntt-context _ _ gf k) foc)
    (_nttz (gf context) k (λ (new-k) (_nttz context (copy-tag-if-present foc (make-ntt-context gf new-k)) up))))

  (define (nttz-down-apply tz i)
    (match-define (nttz context foc up) tz)
    (match-define (ntt-apply _ a cs f) foc)
    (define-values (before i+after) (split-at cs i))
    (match-define (cons c_i after) i+after)
    (_nttz context c_i
           (λ (new-i) (_nttz context (copy-tag-if-present foc (make-ntt-apply a (append before (cons new-i after)) f)) up))))

  (define (nttz-done? tz)
    (ntt-done? (nttz-focus tz)))

  (define (ntac-proc ty ps)
    (let ()
      (define ctxt (mk-empty-ctx))
      (define init-pt
        (new-proof-tree (cur-expand ty)))
      (define final-pt
        (eval-proof-script
         init-pt
         ps
         ctxt
         ps))
      (define pf
        (proof-tree->complete-term
         final-pt
         ps))
;      (pretty-print (syntax->datum pf))
      pf))

  (define (eval-proof-script pt psteps ctxt [err-stx #f])
    (qed (eval-proof-steps (make-nttz pt ctxt) psteps) err-stx))

  (define (eval-proof-steps ptz psteps)
    (for/fold ([nttz ptz])
              ([pstep-stx (in-stx-list psteps)])
      (when (and (current-tracing?)
                 (not (equal? 'display-focus (syntax-e pstep-stx))))
        ((proof-step-inspector) (current-tracing?) pstep-stx nttz)
        (current-tracing? (add1 (current-tracing?))))
      (eval-proof-step nttz pstep-stx)))

  (define (eval-proof-step nttz pstep-stx)
    ;; XXX Error handling on eval
    ;; XXX Namespace is wrong
    (define pstep (eval pstep-stx))
    ;; XXX Error handling on what pstep is and what it returns
    (pstep nttz))

  (define (next tz)
    (match (nttz-focus tz)
      [(ntt-hole _ _) tz]
      [(ntt-exact _ _ _) (next (nttz-up tz))]
      [(ntt-context hole? _ _ k)
       (next (if hole? (nttz-down-context tz) (nttz-up tz)))]
      [(ntt-apply _ _ cs _)
       (next
        (or
         (for/or ([i (in-naturals)]
                  [c (in-list cs)])
           (if (ntt-contains-hole? c)
               (nttz-down-apply tz i)
               #f))
         (nttz-up tz)))]
      [(ntt-done _ _ _)
       tz]))

  (define (qed nttz [err-stx #f])
    (define up-nttz (next nttz))
    (unless (nttz-done? up-nttz)
      (raise-syntax-error 'qed "Proof incomplete.\n" err-stx))
    (nttz-focus up-nttz))

  (define anchor #'a)
  (define (ntac-syntax syn)
    (datum->syntax anchor (syntax->datum syn)))

  (define current-tracing? (make-parameter #f)) ; counts (roughly) # tactics evaled
  (define proof-step-inspector (make-parameter (λ (step-num tactic-stx nttz-pre) (void)))) ; Only called when current-tracing is enabled

  ;; `name` is the binder (thm name); `ty` is the surface stx of the thm
  ;; this is needed bc `name` is likely bound to a normalized
  ;; (and expanded) version of `ty`
  ;  (struct theorem-info identifier-info (name orig))
  )

;; Syntax
(define-syntax (define-theorem stx)
  (syntax-parse stx
    [(_ x:id ty ps ...)
     #:with e (local-expand (ntac-proc #'ty #'(ps ...)) 'expression null)
     (quasisyntax/loc stx (define x e))]))

(define-syntax (define-theorem/for-export stx)
  (syntax-parse stx
    [(_ x:id ty ps ...)
     (quasisyntax/loc stx (define x (ntac ty ps ...)))]))

;; For inline ntac
(define-syntax ntac
  (syntax-parser
    [(_ ty . pf) (ntac-proc #'ty #'pf)]))

;; For ntac/debug
(define-for-syntax (debug-proof-inspector step-num tactic-stx nttz-pre)
  (printf "****************************************\n")
  (printf "step #~a: running tactic: ~a\n"
          step-num
          (syntax->datum tactic-stx)))

;; For inline ntac
(define-syntax ntac/debug
  (syntax-parser
    [(_ ty . pf)
     (parameterize
         ([current-tracing? 1]
          [proof-step-inspector debug-proof-inspector])
       (ntac-proc #'ty #'pf))]))
         
