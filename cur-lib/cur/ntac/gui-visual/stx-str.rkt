#lang cur/metantac

(provide (for-syntax stx->str ctx->str apply->str))

(require "../ctx.rkt"
         "../standard.rkt"
         (for-syntax racket/string  "../standard.rkt"))


(define-for-syntax (stx->str t)
  (define ty-datum (stx->datum (resugar-type t)))
  (define ostr (open-output-string))
  (display ty-datum ostr)
  (get-output-string ostr))

(define-for-syntax (ctx->str ctx)
  (define ids (ctx-ids ctx))
  (define tys (ctx-types ctx))
  (define strs (map (λ (id ty)
                      (string-append "(" (stx->str id) " : " (stx->str ty) ")"))
                    ids tys))
  (string-join strs " "))

(define-for-syntax (apply->str subterms tactic)
  (define fake-subterms
    (map (λ (subterm idx)
           (define expected-ty (ntt-goal subterm))
           (syntax-property #`(Subterm #,idx) (stx->datum #':) expected-ty))
         subterms (range (length subterms))))
  (stx->str (apply tactic fake-subterms)))
