#lang s-exp "../../main.rkt"

(require "../base.rkt"
         "ntt-focus.rkt"
         "../navigate.rkt"
         (for-syntax racket/match))

(begin-for-syntax

  (provide tag-untagged-ntt-with
           tag-untagged-nttz-with)
  
  (define (tag-untagged-ntt-with ntt tag-with)
    ; First, do recursive call on any subterms
    (define sub-tagged-ntts
      (match ntt
        [(ntt-hole _ _) ntt]
        [(ntt-exact _ _ _) ntt]
        [(ntt-context contains-hole? goal env-transformer subtree)
         (_ntt-context contains-hole? goal env-transformer (tag-untagged-ntt-with subtree tag-with))]
        [(ntt-apply contains-hole? goal subterms tactic)
         (_ntt-apply contains-hole? goal (map (λ (subterm) (tag-untagged-ntt-with subterm tag-with)) subterms) tactic)]
        [(ntt-done contains-hole? goal subtree)
         (_ntt-done contains-hole? goal (tag-untagged-ntt-with subtree tag-with))]))

    ; If the root was previously tagged, copy that over. Otherwise, set the new one
    (define new-tag (if (is-tagged-ntt? ntt) (ntt-get-tag ntt) tag-with))
    (ntt-set-tag sub-tagged-ntts new-tag))

  (define (tag-untagged-nttz-with ntz tag-with)
    (define nttx (nttz->ntt-ext ntz))
    
    ; Technically returns a maybe path, but this should never actually return
    ; false since the ntt-ext is genereated directly from a nttz
    (define nav-path (ntt-ext->navigate-focus-path nttx)) 

    (define top-nttz (ntt-ext-this-nttz nttx))
    (define top-ntt (nttz-focus top-nttz))
    (define tagged-top-ntt (tag-untagged-ntt-with top-ntt tag-with))
    (define tagged-top-nttz (make-nttz tagged-top-ntt))
    (navigate-nttz nav-path tagged-top-nttz))
  )