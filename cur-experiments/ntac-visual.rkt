#lang cur
(require
 cur/ntac/base
 cur/ntac/standard
 cur/ntac/focus-tree
 cur/stdlib/nat)

(ntac Nat
      (fill (exact #'4))
      display-focus-tree
      display-focus-tree)

#;(ntac (Π (x : Type) (n : x) x)
      (by-intros x n)
      display-focus-tree
      (by-assumption))