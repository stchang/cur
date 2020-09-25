#lang cur/metantac
;#lang s-exp "../main.rkt"

(require 
  (for-syntax racket/base))

(provide
 (for-syntax display-focus-tree))

(begin-for-syntax 
  (define (display-focus-tree current-nttz)
    (with-handlers ([exn:fail?
                     (λ (e) (error 'display-focus-tree "tactic is only supported when running from command-line"))])
      (eval #`(require racket/gui/base)))
    (eval #`(require cur/ntac/gui-visual/gui))
    (eval #`(test-frame #,current-nttz))))
