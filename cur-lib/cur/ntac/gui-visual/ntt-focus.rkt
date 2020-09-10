#lang cur/metantac

(require (for-syntax racket/base "../base.rkt"))

(begin-for-syntax
  (provide
   make-ntt-focus
   (struct-out ntt-focus))
  
  ; A focused-ntt is a ntt or a (ntt-focus bool type print-ntt)
  ; ntt-focus represents the currently focused subtree of a ntt
  ; Not useful for processing but nice for printing
  ; CONSTRAINT: A focused-ntt will only have one ntt-focus struct in the entire tree
  (struct ntt-focus ntt (subtree) #:transparent #:constructor-name _ntt-focus)
  (define (make-ntt-focus subtree)
    (_ntt-focus (ntt-contains-hole? subtree) (ntt-goal subtree) subtree)))