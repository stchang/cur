#lang cur/metantac

(provide (for-syntax stx->str))

(define-for-syntax (stx->str t)
  (define ty-datum (stx->datum (resugar-type t)))
  (define ostr (open-output-string))
  (display ty-datum ostr)
  (get-output-string ostr))