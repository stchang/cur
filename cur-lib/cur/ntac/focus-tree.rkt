#lang cur/metantac
;#lang s-exp "../main.rkt"

(require ; "./base.rkt"
  "./gui-visual/ntt-focus.rkt"
  (for-syntax syntax/parse racket/base))

(provide
 (for-syntax display-focus-tree
             ))

(begin-for-syntax
 

  
  (define (display-focus-tree current-nttz)
    (define focused-ntt (nttz->ntt-ext current-nttz))
    (displayln focused-ntt)
    (with-handlers ([exn:fail?
                     (λ (e) (error 'display-focus-tree "tactic is only supported when running from command-line"))])
      (eval #`(require racket/gui/base)))
    (eval #`(require cur/ntac/gui-visual/gui))
    (eval #`(test-frame #,current-nttz #,focused-ntt))))
