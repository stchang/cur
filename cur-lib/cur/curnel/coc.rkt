#lang racket/base
(require (for-syntax racket/base syntax/parse
                     (for-syntax racket/base)))

(provide (for-syntax ~Π))
(struct Π- (X))
(begin-for-syntax
  (define TY/internal+ (local-expand #'Π- 'expression null))
  (define-syntax ~Π
    (pattern-expander (λ (stx) #'(~do TY/internal+)))))
