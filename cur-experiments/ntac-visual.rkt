#lang cur
(require
 cur/ntac/base
 cur/ntac/focus-tree)

(ntac (Π (x : Type) (n : x) x)
      (by-intros x n)
      display-focus-tree
      (by-assumption))