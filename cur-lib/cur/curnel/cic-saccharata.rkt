#lang racket/base
(require (for-syntax racket/base syntax/parse)
         "coc.rkt")
(provide #%module-begin)
(define-for-syntax nested-positivity? (syntax-parser [(~Π (x : A) B) (void)]))
