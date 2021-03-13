#lang cur

(require
  cur/ntac/base
  cur/ntac/standard
  cur/ntac/focus-tree
  cur/stdlib/nat
  cur/stdlib/equality
  cur/ntac/standard
  cur/ntac/rewrite
  cur/ntac/f-equal
  cur/tests/ntac/rackunit-ntac)

(define add1+=+add1
  (ntac/visual (Π (n : Nat)
           (j : Nat)
           (== Nat
               (s (plus n j))
               (plus n (s j))))
        (by-intros n j)
        (by-induction n)
        reflexivity
;        (by-apply f-equal #:with Nat Nat s (s (plus X1 j)) (plus X1 (s j)))
;        (f-equal-tac #:with Nat Nat s (s (plus X1 j)) (plus X1 (s j)))
        f-equal-tac
        by-assumption))
