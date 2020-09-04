#lang cur/metantac
;#lang s-exp "../main.rkt"

(require ; "./base.rkt"
         "./gui-visual/ntt-focus.rkt"
         (for-syntax syntax/parse racket/base))

(provide
 (for-syntax display-focus-tree
             nttz->focused-ntt))

(begin-for-syntax
 
  ; nttz -> (ntt, ntt)
  ; Returns the root of the tree, and the currently focused node
  ; One node in the tree should eq? the focus
  (define (get-full-tree-and-focus tz)
    (define focus (nttz-focus tz))
    (define top (to-top tz))
    (values (nttz-focus top) focus))

  ; ntt ntt -> focused-ntt
  ; Insert the focus into the ntt
  (define (insert-focus-ntt tree focus)
    (parameterize ([current-tracing? #t])
      (let loop [(pt tree)]
        (if (eq? pt focus)
            (make-ntt-focus pt) ; Don't recursively call
            (cond 
              [(ntt-hole? pt) pt]
              [(ntt-exact? pt) pt]
              [(ntt-context? pt) (make-ntt-context (ntt-context-env-transformer pt) (loop (ntt-context-subtree pt)))]
              [(ntt-apply? pt) (make-ntt-apply (ntt-goal pt) (map loop (ntt-apply-subterms pt)) (ntt-apply-tactic pt))]
              [(ntt-done? pt) (make-ntt-done (loop (ntt-done-subtree pt)))])))))

  ; nttz -> focused-ntt
  ; Get the whole tree, but with one part focused
  (define (nttz->focused-ntt nttz)
    (define-values (top focus) (get-full-tree-and-focus nttz))
    (insert-focus-ntt top focus))
  
  (define (display-focus-tree current-nttz)
    (define focused-ntt (nttz->focused-ntt current-nttz))
    (eval #`(require cur/ntac/gui-visual/gui))
    (eval #`(test-frame #,current-nttz #,focused-ntt))))
