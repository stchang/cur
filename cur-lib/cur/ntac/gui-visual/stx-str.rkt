#lang cur/metantac

(provide (for-syntax stx->str ctx->str))

(require "../ctx.rkt"
         (for-syntax racket/string))

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