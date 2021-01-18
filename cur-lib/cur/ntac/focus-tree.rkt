#lang cur/metantac
;#lang s-exp "../main.rkt"

(require 
  (for-syntax racket/base racket/port "./gui-visual/interaction-history.rkt")
  "./gui-visual/stx-str.rkt"
  "./gui-visual/ntt-tag.rkt")

(provide
 (for-syntax display-focus-tree )
 ntac/visual)

(define-syntax (ntac/visual stx)
  (syntax-parse stx
    [(_ ty . pf) (ntac-proc/visual #'ty #'pf)]))

(begin-for-syntax

  (define (display-focus-tree current-nttz)
    (display-focus-tree/interaction-history current-nttz '(list)))
  
  (define (display-focus-tree/interaction-history current-nttz history)
    (with-handlers ([exn:fail?
                     (λ (e) (error 'display-focus-tree "tactic is only supported when running from command-line"))])
      (eval #`(require racket/gui/base)))
    (eval #`(require cur/ntac/gui-visual/gui))
    (eval #`(test-frame #,current-nttz (list #,@history))))
  
  (define (ntac-proc/visual ty ps)
    (let ()
      (define ctxt (mk-empty-ctx))
      (define init-pt
        (new-proof-tree (cur-expand ty)))
      (define-values (pt-before-visual preload-interaction)
        (eval-proof-steps/interaction-history (make-nttz init-pt ctxt) ps))
      (define pt-after-visual (display-focus-tree/interaction-history pt-before-visual preload-interaction))
      (define pf
        (proof-tree->complete-term
         (qed pt-after-visual ps)
         ps))
      ;      (pretty-print (syntax->datum pf))
      pf))

  (define (eval-proof-script pt psteps ctxt [err-stx #f])
    (qed (eval-proof-steps (make-nttz pt ctxt) psteps) err-stx))

  (define (eval-proof-steps/interaction-history ptz psteps)
    (for/fold ([nttz (tag-untagged-nttz-with ptz "Initial")] ; TOOD Tag with something better than just the string
               [interaction-hist '()])
              ([pstep-stx (in-stx-list psteps)])
      (define pstep-str (stx->str pstep-stx)) ; Intentionally break some scoping rules by converting to string.
      (define pstep-back-again (eval (with-input-from-string pstep-str read-syntax))) ; There has to be a better way.
      (define next-step (tag-untagged-nttz-with (pstep-back-again nttz) pstep-str))
      (values next-step (cons (interaction-history nttz next-step pstep-str #f) interaction-hist)))))
