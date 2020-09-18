#lang cur/metantac

(require (for-syntax racket/base
                     "../base.rkt"))

(require "../navigate.rkt")

(begin-for-syntax
  (provide
   nttz->ntt-ext
   (struct-out ntt-ext)
   (struct-out ntt-ext-leaf)
   (struct-out ntt-ext-node)
   )

  (struct ntt-ext (is-focus? on-path-to-focus? path-to-here this-nttz))

  ; A ntt-ext is one of the following, it is sort of a combination of a ntt and nttz.
  ; It has a lot of cached information about where it is in the tree, along with if it is on the path to the focus
  (struct ntt-ext-leaf ntt-ext () #:transparent #:constructor-name _ntt-ext-leaf)
  (struct ntt-ext-node ntt-ext (subtrees) #:transparent #:constructor-name _ntt-ext-node)

  (define (make-ntt-ext-leaf focused? path-inv this-nttz)
    (_ntt-ext-leaf focused? focused? (reverse path-inv) this-nttz))

  (define (make-ntt-ext-node focused? path-inv this-nttz subtrees)
    (define any-subtrees-focused? (ormap (λ (tree) (ntt-ext-on-path-to-focus? tree)) subtrees))
    (_ntt-ext-node focused? (or focused? any-subtrees-focused?) (reverse path-inv) this-nttz subtrees))

  ; Pre-calculate a lot of interesting properties about a ntt
  ; Makes it more suitable for the gui
  (define (nttz->ntt-ext nttz)
    (define-values (top-tree focus) (get-full-tree-and-focus nttz)) ; Intermediate step: Get top-level view of where the focus is
    (ntt+focus->ntt-ext top-tree focus))

  (define (ntt+focus->ntt-ext top-ntt focus)
    (let loop ([current-nttz (make-nttz top-ntt)] ; Do we need ctx?
               [current-path-inv '()])
      (match-define (nttz _ current-ntt _) current-nttz)
      (define focused? (eq? current-ntt focus))
      (match current-ntt
        [(ntt-done _ _ sub)
         (define sub-ext (loop (nttz-down-done current-nttz) (cons (path-down-done) current-path-inv)))
         (make-ntt-ext-node focused? current-path-inv current-nttz (list sub-ext))]
        [(ntt-hole _ _) (make-ntt-ext-leaf focused? current-path-inv current-nttz)]
        [(ntt-exact _ _ _) (make-ntt-ext-leaf focused? current-path-inv current-nttz)]
        [(ntt-context _ _ _ sub)
         (define sub-ext (loop (nttz-down-context current-nttz) (cons (path-down-context) current-path-inv)))
         (make-ntt-ext-node focused? current-path-inv current-nttz (list sub-ext))]
        [(ntt-apply _ _ subs _)
         (define sub-exts (map (λ (subterm position) (loop (nttz-down-apply current-nttz position) (cons (path-down-apply position) current-path-inv)))
                               subs (range (length subs))))
         (make-ntt-ext-node focused? current-path-inv current-nttz sub-exts)])))

  ; nttz -> (ntt, ntt)
  ; Returns the root of the tree, and the currently focused node
  ; One node in the tree should eq? the focus
  (define (get-full-tree-and-focus tz)
    (define focus (nttz-focus tz))
    (define top (to-top tz))
    (values (nttz-focus top) focus)))